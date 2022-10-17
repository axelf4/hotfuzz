/**
 * C implementation of the hotfuzz completion style.
 *
 * See the Lisp source for an explanation of the algorithm.
 */
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdalign.h>
#include <string.h>
#include <emacs-module.h>
#include <pthread.h>
#include <sys/sysinfo.h>

#define MIN(a, b) ({ __typeof__(a) _a = (a), _b = (b); _a < _b ? _a : _b; })
#define MAX(a, b) ({ __typeof__(a) _a = (a), _b = (b); _a > _b ? _a : _b; })

#define MAX_NEEDLE_LEN 128
#define MAX_HAYSTACK_LEN 512
#define BATCH_SIZE 2048

int plugin_is_GPL_compatible;

/** An Emacs string made accessible by copying. */
struct EmacsStr {
	emacs_value value; ///< The original string value.
	size_t len; ///< The length of the string minus the null byte.
	char b[]; ///< The null-terminated copied string.
};

static char tolower_utf8(char c) {
	return *u8"A" <= c && c <= *u8"Z" ? c + (*u8"a" - *u8"A") : c;
}

static char toupper_utf8(char c) {
	return *u8"a" <= c && c <= *u8"z" ? c - (*u8"a" - *u8"A") : c;
}

static uint64_t tolower8(uint64_t x) {
	uint64_t ones = 0x0101010101010101,
		is_gt_Z = (0x7f * ones & x) + (0x7f - *u8"Z") * ones,
		is_ge_A = (0x7f * ones & x) + (0x80 - *u8"A") * ones,
		is_upper = 0x80 * ones & ~x & (is_ge_A ^ is_gt_Z);
	return x | is_upper >> 2;
}

static void strtolower(struct EmacsStr *s) {
	// Complicated in order to optimize out the calls to tolower_utf8
	// on AMD64 System V with GCC 11.3.0.

	size_t i = 0;
	if ((alignof(struct EmacsStr) + offsetof(struct EmacsStr, b)) % alignof(uint64_t))
		for (; ((uintptr_t) s->b + i) % alignof(uint64_t) && i < s->len; ++i)
			s->b[i] = tolower_utf8(s->b[i]);

	size_t pad = alignof(struct EmacsStr)
		- offsetof(struct EmacsStr, b) % alignof(struct EmacsStr);
	for (; i + 8 < s->len + MIN(pad, 8); i += 8) {
		uint64_t *x = (uint64_t *) (s->b + i);
		*x = tolower8(*x);
	}

	if (pad < 8 - 1) for (; i < s->len; ++i) s->b[i] = tolower_utf8(s->b[i]);
}

typedef int cost;

static cost char_bonus(char prev, char ch) {
	cost word_bonus = 80;
	switch (ch) {
	case 'A' ... 'Z':
		if ('a' <= prev && prev <= 'z') return word_bonus;
		// Intentional fallthrough
	case 'a' ... 'z':
		switch (prev) {
		case '/': return 90;
		case '.': return 60;
		case '-': case '_': case ' ': return word_bonus;
		}
		// Intentional fallthrough
	default:
		return 0;
	}
}

static void calc_bonus(struct EmacsStr *haystack, cost *out) {
	char ch, lastch = '/';
	for (size_t i = 0; i < haystack->len; ++i, lastch = ch)
		out[i] = char_bonus(lastch, ch = haystack->b[i]);
}

static void match_row(struct EmacsStr *a, struct EmacsStr *b, cost *bonuses, unsigned i,
	cost *nc, cost *nd, cost *pc, cost *pd) {
	cost g = 100, h = 5;
	size_t m = b->len;
	cost oldc, s = i ? g + h * i : 0;
	for (size_t j = 0; j < m; ++j, s = oldc) {
		oldc = pc[j];
		nc[j] = MIN(nd[j] = MIN(pd[j], oldc + g) + (j == m - 1 ? h : 2 * h),
			a->b[i] == b->b[j] ? s - bonuses[i] : 100000);
	}
}

static cost get_cost(struct EmacsStr *needle, struct EmacsStr *haystack, bool ignore_case) {
	unsigned n = haystack->len, m = needle->len;
	if (n > MAX_HAYSTACK_LEN || m > MAX_NEEDLE_LEN) return 10000;
	cost c[MAX_NEEDLE_LEN], d[MAX_NEEDLE_LEN];
	for (unsigned j = 0; j < m; ++j) c[j] = d[j] = 10000;

	cost bonuses[MAX_HAYSTACK_LEN];
	calc_bonus(haystack, bonuses);

	if (ignore_case) strtolower(haystack);
	for (unsigned i = 0; i < n; ++i)
		match_row(haystack, needle, bonuses, i, c, d, c, d);

	return c[m - 1];
}

/**
 * Returns whether @p haystack matches @p needle.
 *
 * @param needle Null-terminated search string.
 * @param haystack Null-terminated completion candidate.
 * @param ignore_case Whether to match case-insensitively.
 */
static bool is_match(char *needle, char *haystack, bool ignore_case) {
	while (*needle) {
		if (ignore_case
			? (haystack = strpbrk(haystack, (char[]) { *needle, toupper_utf8(*needle), '\0' }))
			: (haystack = strchr(haystack, *needle)))
			++needle, ++haystack; // Skip past matched character
		else
			return false;
	}
	return true;
}

/** Intrusive linked list of bump allocation blocks. */
struct Bump {
	struct Bump *next;
	size_t index, capacity;
	char b[];
};

/**
 * Allocates the specified number of bytes.
 *
 * Returns NULL on failure.
 */
static void *bump_alloc(struct Bump **head, size_t len) {
	if (!*head || (*head)->capacity - (*head)->index < len) {
		size_t capacity = MAX(*head ? 2 * (*head)->capacity : 1024, len);
		struct Bump *new_head;
		if (!(new_head = malloc(sizeof *new_head + capacity)))
			return NULL;
		*new_head = (struct Bump) { .next = *head, .index = 0, .capacity = capacity };
		*head = new_head;
	}

	void *p = (*head)->b + (*head)->index;
	(*head)->index += len;
	return p;
}

static void bump_free(struct Bump *head) {
	while (head) {
		struct Bump *next = head->next;
		free(head);
		head = next;
	}
}

/**
 * Copies the Emacs string to make its lifetime that of the allocator.
 */
static struct EmacsStr *copy_emacs_string(emacs_env *env, struct Bump **bump, emacs_value value) {
	ptrdiff_t len;
	// Determine the size of the string (including null-terminator)
	env->copy_string_contents(env, value, NULL, &len);

	struct EmacsStr *result;
	// Note: Since only EmacsStr:s are allocated with bump_alloc we
	// may use its smaller alignment rather than the scalar maximum.
	if (!(result = bump_alloc(bump, sizeof *result + len
				+ alignof(struct EmacsStr) - 1 & ~(alignof(struct EmacsStr) - 1))))
		return NULL;

	result->value = value;
	result->len = len - 1;
	env->copy_string_contents(env, value, result->b, &len);
	return result;
}

struct Candidate {
	struct EmacsStr *s;
	cost key;
};

static int cmp_candidate(const void *a, const void *b) {
	return ((struct Candidate *) a)->key - ((struct Candidate *) b)->key;
}

struct Batch {
	unsigned len;
	struct Candidate xs[BATCH_SIZE];
};

struct Shared {
	pthread_mutex_t mutex;
	bool ignore_case;
	struct EmacsStr *needle;
	struct Batch *batches, *batches_end;
};

struct Worker {
	pthread_t thread;
	struct Shared *shared;
	struct Batch *batch; ///< The initial batch to work on.
};

static enum JobRetVal {
	JOB_FINISHED,
	JOB_FAILED
} job_finished = JOB_FINISHED, job_failed = JOB_FAILED;

static void *worker_routine(void *ptr) {
	struct Worker *worker = ptr;
	struct Shared *shared = worker->shared;
	struct EmacsStr *needle = shared->needle;
	struct Batch *batch = worker->batch;

	do {
		unsigned num_matches = 0;
		for (unsigned i = 0; i < batch->len; ++i) {
			struct Candidate *candidate = batch->xs + i;
			if (!is_match(needle->b, candidate->s->b, shared->ignore_case)) continue;
			batch->xs[num_matches++] = (struct Candidate) {
				.s = candidate->s,
				.key = get_cost(needle, candidate->s, shared->ignore_case),
			};
		}
		batch->len = num_matches;

		// Try to fetch a new batch
		if (pthread_mutex_lock(&shared->mutex)) return &job_failed;
		batch = shared->batches < shared->batches_end ? shared->batches++ : NULL;
		pthread_mutex_unlock(&shared->mutex);
	} while (batch);

	return &job_finished;
}

/** Module userdata that gets allocated once at initialization. */
struct Data {
	unsigned max_workers;
	struct Worker *workers;
};

emacs_value hotfuzz_filter(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data_ptr) {
	struct Data *data = data_ptr;
	emacs_value fcar = env->intern(env, "car"),
		fcdr = env->intern(env, "cdr"),
		fcons = env->intern(env, "cons"),
		nil = env->intern(env, "nil");
	struct Bump *bump = NULL;
	int success = false;
	emacs_value result = nil;

	// Collect all candidates
	emacs_value list = args[1];
	struct Batch *batches = NULL;
	size_t batch_idx = 0, capacity = 0;
	while (env->is_not_nil(env, list)) {
		if ((batches && batches[batch_idx].len >= BATCH_SIZE ? ++batch_idx : batch_idx)
			>= capacity) {
			capacity = capacity ? 2 * capacity : 1;
			struct Batch *new_batches;
			if (!(new_batches = realloc(batches, capacity * sizeof *batches)))
				goto error;
			batches = new_batches;
			for (size_t i = batch_idx; i < capacity; ++i)
				batches[i].len = 0;
		}

		emacs_value value = env->funcall(env, fcar, 1, (emacs_value[]) {list});
		struct Batch *b = batches + batch_idx;
		if (!(b->xs[b->len++].s = copy_emacs_string(env, &bump, value)))
			goto error;
		list = env->funcall(env, fcdr, 1, (emacs_value[]) {list});
	}
	if (!batches) return nil;

	bool ignore_case = nargs >= 3 && env->is_not_nil(env, args[2]);
	struct EmacsStr *needle = copy_emacs_string(env, &bump, args[0]);
	if (!needle) goto error;
	if (ignore_case)
		for (size_t i = 0; i < needle->len; ++i)
			needle->b[i] = tolower_utf8(needle->b[i]);
	struct Shared shared = {
		.ignore_case = ignore_case,
		.needle = needle,
		.batches = batches,
		.batches_end = batches + batch_idx + 1,
	};
	if (pthread_mutex_init(&shared.mutex, NULL)) goto error;
	if (pthread_mutex_lock(&shared.mutex)) goto mutex_error;
	enum JobRetVal res = job_finished;
	unsigned worker_count;

	struct Worker *workers = data->workers;
	for (worker_count = 0; worker_count < data->max_workers
				&& shared.batches < shared.batches_end; ++worker_count) {
		struct Worker *worker = workers + worker_count;
		*worker = (struct Worker) {
			.shared = &shared,
			.batch = shared.batches++,
		};

		if (pthread_create(&worker->thread, NULL, worker_routine, worker)) {
			// Join all workers in order to at least safely destroy mutex
			res = job_failed;
			break;
		}
	}
	pthread_mutex_unlock(&shared.mutex);

	// Wait for all worker threads
	for (unsigned i = 0; i < worker_count; ++i) {
		enum JobRetVal *retval;
		pthread_join(workers[i].thread, (void **) &retval);
		res |= *retval;
	}
	if (res != job_finished) goto mutex_error;

	success = true;
	if (env->process_input(env) == emacs_process_input_quit) goto mutex_error;

	// Compact all batches
	size_t len = batches[0].len;
	struct Candidate *xs = batches[0].xs;
	for (struct Batch *b = batches + 1; b < shared.batches_end; ++b) {
		unsigned n = b->len;
		memmove(xs + len, b->xs, n * sizeof *b->xs);
		len += n;
	}
	qsort(xs, len, sizeof *xs, cmp_candidate); // Sort the completions

	for (size_t i = len; i-- > 0;)
		result = env->funcall(env, fcons, 2, (emacs_value[]) {xs[i].s->value, result});

mutex_error:
	pthread_mutex_destroy(&shared.mutex);
error:
	free(batches);
	bump_free(bump);

	if (!success)
		env->non_local_exit_signal(env, env->intern(env, "error"), nil);
	return result;
}

int emacs_module_init(struct emacs_runtime *rt) {
	// Verify compatability with Emacs executable loading this module
	if ((size_t) rt->size < sizeof *rt)
		return 1;
	emacs_env *env = rt->get_environment(rt);
	if ((size_t) env->size < sizeof *env)
		return 2;

	static struct Data data;
	data.max_workers = get_nprocs();
	if (!(data.workers = malloc(data.max_workers * sizeof *data.workers)))
		return 1;

	env->funcall(env, env->intern(env, "defalias"), 2, (emacs_value[]) {
			env->intern(env, "hotfuzz--filter-c"),
			env->make_function(env, 2, 3, hotfuzz_filter,
				"Filter and sort CANDIDATES that match STRING.\n"
				"\n"
				"\(fn STRING CANDIDATES &optional IGNORE-CASE)",
				&data),
		});

	env->funcall(env, env->intern(env, "provide"), 1,
		(emacs_value[]) { env->intern(env, "hotfuzz-module") });

	return 0;
}
