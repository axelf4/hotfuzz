/**
 * C implementation of the hotfuzz completion style.
 *
 * See the Lisp source for an explanation of the algorithm.
 */
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <pthread.h>
#include <stdio.h>
#include <emacs-module.h>
#include <sys/sysinfo.h>

#define MIN(a, b) ({ __typeof__(a) _a = (a), _b = (b); _a < _b ? _a : _b; })
#define MAX(a, b) ({ __typeof__(a) _a = (a), _b = (b); _a > _b ? _a : _b; })

#define MAX_NEEDLE_LEN 128
#define MAX_HAYSTACK_LEN 512
#define BATCH_SIZE 1024

int plugin_is_GPL_compatible;

struct CharSlice { size_t len; char *buf; };

typedef int cost;

static cost charBonus(char prev, char ch) {
	cost wordBonus = 80;
	switch (ch) {
	case 'A' ... 'Z':
		if ('a' <= prev && prev <= 'z') return wordBonus;
		// Intentional fallthrough
	case 'a' ... 'z':
		switch (prev) {
		case '/': return 90;
		case '.': return 60;
		case '-': case '_': case ' ': return wordBonus;
		}
		// Intentional fallthrough
	default:
		return 0;
	}
}

static void calcBonus(struct CharSlice haystack, cost *out) {
	char ch, lastch = '/';
	for (size_t i = 0; i < haystack.len; ++i, lastch = ch)
		out[i] = charBonus(lastch, ch = haystack.buf[i]);
}

/**
 * Returns equality of the two characters up to a difference of case.
 */
static bool charEqual(char a, char b) {
	return tolower(a) == tolower(b);
}

static void matchRow(struct CharSlice a, struct CharSlice b, cost *bonuses, int i,
					 cost *nc, cost *nd, cost *pc, cost *pd) {
	cost g = 100, h = 5;
	size_t m = b.len;
	cost oldc, s = i ? g + h * i : 0;
	for (size_t j = 0; j < m; ++j, s = oldc) {
		oldc = pc[j];
		nc[j] = MIN(nd[j] = MIN(pd[j], oldc + g) + (j == m - 1 ? h : 2 * h),
					charEqual(a.buf[i], b.buf[j]) ? s - bonuses[i] : 100000);
	}
}

static cost getCost(struct CharSlice needle, struct CharSlice haystack) {
	int n = haystack.len, m = needle.len;
	if (n > MAX_HAYSTACK_LEN || m > MAX_NEEDLE_LEN) return 10000;
	cost c[MAX_NEEDLE_LEN], d[MAX_NEEDLE_LEN];
	for (int i = 0; i < MAX_NEEDLE_LEN; ++i) c[i] = d[i] = 10000;

	cost bonuses[MAX_HAYSTACK_LEN];
	calcBonus(haystack, bonuses);

	for (int i = 0; i < n; ++i) matchRow(haystack, needle, bonuses, i, c, d, c, d);

	return c[m - 1];
}

/**
 * Returns whether haystack case-insensitively matches needle.
 *
 * This function does not take the value of completion-ignore-case
 * into account.
 *
 * @param needle Null-terminated search string.
 * @param haystack Null-terminated completion candidate.
 */
static bool isMatch(char *needle, char *haystack) {
	while (*needle) {
		if ((haystack = strpbrk(haystack, (char[]) { tolower(*needle), toupper(*needle), '\0' })))
			++needle, ++haystack; // Skip past matched character
		else
			return false;
	}
	return true;
}

/// Intrusive linked list of string buffer blocks.
struct StrBuf {
	struct StrBuf *next;
	size_t index, capacity;
	char buf[];
};

/**
 * Copies the Emacs string to make its lifetime that of the StrBuf.
 *
 * The resulting string is null-terminated.
 */
static struct CharSlice strBufAdd(emacs_env *env, struct StrBuf **head, emacs_value value) {
	ptrdiff_t len;
	// Determine the size of the string (including null-terminator)
	env->copy_string_contents(env, value, NULL, &len);

	if (!*head || (ptrdiff_t) ((*head)->capacity - (*head)->index) < len) {
		size_t capacity = MAX(2048, len);
		struct StrBuf *newHead;
		if (!(newHead = malloc(sizeof *newHead + capacity)))
			return (struct CharSlice) {0, NULL};
		newHead->next = *head;
		newHead->index = 0;
		newHead->capacity = capacity;
		*head = newHead;
	}

	char *s = (*head)->buf + (*head)->index;
	env->copy_string_contents(env, value, s, &len);
	(*head)->index += len;

	return (struct CharSlice) {len - 1, s};
}

static void strBufFree(struct StrBuf *head) {
	while (head) {
		struct StrBuf *next = head->next;
		free(head);
		head = next;
	}
}

struct Candidate {
	/// The original string value.
	emacs_value value;
	/// The copied string.
	struct CharSlice s;
	cost key;
};

static int cmpCandidate(const void *a, const void *b) {
	return ((struct Candidate *) a)->key - ((struct Candidate *) b)->key;
}

/// A batch is a collection of candidates that needs to be processed.
struct Batch {
	size_t count, capacity;
	struct Candidate candidates[];
};

struct Shared {
	pthread_mutex_t mutex;
	struct CharSlice needle;
	size_t remaining; ///< The remaining number of batches.
	struct Batch *batches; ///< Array of remaining batches.
	size_t matchCount;
};

struct Job {
	pthread_t thread;
	struct Shared *shared;
	struct Batch *batch;
};

static enum JobRetVal {
	JOB_FINISHED,
	JOB_FAILED
} jobFinished = JOB_FINISHED, jobFailed = JOB_FAILED;

static void *filterCandidates(void *ptr) {
	struct Job *job = ptr;
	struct Shared *shared = job->shared;
	struct CharSlice needle = shared->needle;
	struct Batch *batch = job->batch;

	do {
		size_t matchIdx = 0;
		for (size_t i = 0; i < batch->count; ++i) {
			struct Candidate *candidate = batch->candidates + i;
			if (!isMatch(needle.buf, candidate->s.buf)) continue;

			batch->candidates[matchIdx++] = (struct Candidate) {
				.value = candidate->value,
				.s = candidate->s,
				.key = getCost(needle, candidate->s),
			};
		}
		batch->count = matchIdx; // Update number of matching completions

		// Try to get new batch
		if (pthread_mutex_lock(&shared->mutex)) return &jobFailed;
		shared->matchCount += matchIdx;
		if (shared->remaining > 0) {
			--shared->remaining;
			batch = shared->batches;
			shared->batches = (void *) (shared->batches + 1) + sizeof(struct Candidate) * batch->capacity;
		} else {
			batch = NULL;
		}
		pthread_mutex_unlock(&shared->mutex);
	} while (batch);

	return &jobFinished;
}

/// Module userdata that gets allocated once at initialization.
struct Data {
	size_t maxJobs;
	struct Job *jobs;
};

emacs_value filterEmacsList(emacs_env *env, ptrdiff_t nargs __attribute__ ((__unused__)), emacs_value args[], void *dataPtr) {
	struct Data *data = dataPtr;
	emacs_value fcar = env->intern(env, "car"),
		fcdr = env->intern(env, "cdr"),
		fcons = env->intern(env, "cons"),
		nil = env->intern(env, "nil");
	struct StrBuf *strBufs = NULL;
	int success = false;
	emacs_value result = nil;

	// Collect all candidates
	emacs_value list = args[1];
	struct Batch *batches = NULL;
	size_t batchIdx = 0, capacity = 0;
	while (env->is_not_nil(env, list)) {
		if (batchIdx >= capacity) {
			capacity = MAX(2 * capacity, 1);
			struct Batch *newBatches;
			if (!(newBatches = realloc(batches, capacity * (sizeof *batches + sizeof(struct Candidate) * BATCH_SIZE))))
				goto error;
			batches = newBatches;

			// Initialize the newly allocated batches
			for (size_t i = batchIdx; i < capacity; ++i) {
				struct Batch *batch = (void *) batches + (sizeof *batches + sizeof(struct Candidate) * BATCH_SIZE) * i;
				batch->count = 0;
				batch->capacity = BATCH_SIZE;
			}
		}

		struct Batch *batch = (void *) batches + (sizeof *batches + sizeof(struct Candidate) * BATCH_SIZE) * batchIdx;
		emacs_value value = env->funcall(env, fcar, 1, (emacs_value[]) {list});
		struct CharSlice s = strBufAdd(env, &strBufs, value);
		if (!s.buf) goto error;
		batch->candidates[batch->count++] = (struct Candidate) { .value = value, .s = s, };
		if (batch->count >= BATCH_SIZE) ++batchIdx;

		list = env->funcall(env, fcdr, 1, (emacs_value[]) {list});
	}

	struct CharSlice needle = strBufAdd(env, &strBufs, args[0]);
	if (!needle.buf) goto error;
	struct Shared shared = {
		.needle = needle,
		.remaining = batchIdx + 1,
		.batches = batches,
		.matchCount = 0,
	};
	if (pthread_mutex_init(&shared.mutex, NULL)) goto error;
	if (pthread_mutex_lock(&shared.mutex)) goto mutex_error;

	enum JobRetVal res = jobFinished;
	size_t jobCount;
	struct Job *jobs = data->jobs;
	for (jobCount = 0; jobCount < data->maxJobs && shared.remaining; ++jobCount, --shared.remaining) {
		struct Job *job = jobs + jobCount;
		*job = (struct Job) {
			.shared = &shared,
			.batch = shared.batches,
		};
		shared.batches = (void *) (shared.batches + 1) + sizeof(struct Candidate) * job->batch->capacity;

		if (pthread_create(&job->thread, NULL, filterCandidates, job)) {
			// Join all worker threads in order to at least safely destroy mutex
			res = jobFailed;
			break;
		}
	}
	pthread_mutex_unlock(&shared.mutex);

	// Wait for all worker threads
	for (size_t i = 0; i < jobCount; ++i) {
		enum JobRetVal *retval;
		pthread_join(jobs[i].thread, (void **) &retval);
		res |= *retval;
	}
	if (res != jobFinished) goto mutex_error;

	// Compact all batches to be able to sort
	struct Batch *batch = batches,
		*b = (void *) (batch + 1) + sizeof(struct Candidate) * batch->capacity;
	size_t k = batch->count;
	for (size_t i = 1; i <= batchIdx; ++i) {
		size_t count = b->count, capacity = b->capacity;
		for (size_t j = 0; j < count; ++j)
			batch->candidates[k++] = b->candidates[j];
		batch->count += count;
		batch->capacity += capacity;

		b = (void *) (b + 1) + sizeof(struct Candidate) * capacity;
	}

	// Sort the completions
	if (needle.len > 0)
		qsort(batch->candidates,
			  batch->count,
			  sizeof *batch->candidates,
			  cmpCandidate);

	for (size_t i = batch->count; i-- > 0;)
		result = env->funcall(env, fcons, 2,
							  (emacs_value[]) {batch->candidates[i].value, result});
	success = true;

mutex_error:
	pthread_mutex_destroy(&shared.mutex);
error:
	free(batches);
	strBufFree(strBufs);

	if (!success)
		env->non_local_exit_signal(env, env->intern(env, "error"), nil);
	return result;
}

int emacs_module_init(struct emacs_runtime *ert) {
	emacs_env *env = ert->get_environment(ert);

	long numProcs = get_nprocs();
	static struct Data data;
	data.maxJobs = numProcs;
	if (!(data.jobs = malloc(data.maxJobs * sizeof *data.jobs)))
		return 1;

	env->funcall(env, env->intern(env, "defalias"),
				 2, (emacs_value[]) {
					 env->intern(env, "hotfuzz--filter-c"),
					 env->make_function(env, 2, 2, filterEmacsList,
										"Filter and sort CANDIDATES that match STRING.\n"
										"\n"
										"\(fn STRING CANDIDATES)",
										&data),
				 });

	env->funcall(env, env->intern(env, "provide"), 1,
				 (emacs_value[]) { env->intern(env, "hotfuzz-module") });

	return 0;
}
