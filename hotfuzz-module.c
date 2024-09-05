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
#include <unistd.h>

#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))

#define MAX_NEEDLE_LEN 128
#define MAX_HAYSTACK_LEN 512
#define BATCH_SIZE 2048

int plugin_is_GPL_compatible;

struct Str { char *b; size_t len; };

static char toupper_utf8(char c) {
	return *u8"a" <= c && c <= *u8"z" ? c - (*u8"a" - *u8"A") : c;
}
static void strtolower(struct Str s) {
	uint64_t ones = ~UINT64_C(0) / 0xff, x;
	for (size_t i = 0; i < s.len; i += sizeof x) {
		memcpy(&x, s.b + i, sizeof x);
		uint64_t is_gt_Z = (0x7f * ones & x) + (0x7f - *u8"Z") * ones,
			is_ge_A = (0x7f * ones & x) + (0x80 - *u8"A") * ones,
			is_upper = 0x80 * ones & ~x & (is_ge_A ^ is_gt_Z);
		x |= is_upper >> 2;
		memcpy(s.b + i, &x, sizeof x);
	}
}

static int char_bonus(char prev, char ch) {
	int word_bonus = 80;
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

static void match_row(struct Str a, struct Str b, int *bonuses, unsigned i, int *c, int *d) {
	int g = 100, h = 5;
	size_t m = b.len;
	int oldc, s = i ? g + h * i : 0;
	for (size_t j = 0; j < m; ++j, s = oldc) {
		oldc = c[j];
		d[j] = MIN(d[j], oldc + g) + (j == m - 1 ? h : 2 * h);
		c[j] = a.b[i] == b.b[j] ? MIN(d[j], s - bonuses[i]) : d[j];
	}
}

static int calc_cost(struct Str needle, struct Str haystack, bool ignore_case) {
	unsigned n = haystack.len, m = needle.len;
	if (n > MAX_HAYSTACK_LEN || m > MAX_NEEDLE_LEN) return 100000;
	int c[MAX_NEEDLE_LEN], d[MAX_NEEDLE_LEN];
	for (unsigned j = 0; j < m; ++j) c[j] = d[j] = 100000;

	int bonuses[MAX_HAYSTACK_LEN];
	char ch, lastch = '/';
	for (unsigned i = 0; i < n; ++i, lastch = ch)
		bonuses[i] = char_bonus(lastch, ch = haystack.b[i]);

	if (ignore_case) strtolower(haystack);
	for (unsigned i = 0; i < n; ++i)
		match_row(haystack, needle, bonuses, i, c, d);

	return c[m - 1];
}

/**
 * Returns whether @a haystack matches @a needle.
 *
 * @param needle Null-terminated search string.
 * @param haystack Null-terminated completion candidate.
 * @param ignore_case Whether to match case-insensitively.
 */
static bool is_match(char *needle, char *haystack, bool ignore_case) {
	while (*needle)
		if ((haystack = ignore_case
				? strpbrk(haystack, (char[]) { *needle, toupper_utf8(*needle), '\0' })
				: strchr(haystack, *needle)))
			++needle, ++haystack; // Skip past matched character
		else
			return false;
	return true;
}

/** Intrusive linked list of bump allocation blocks. */
struct Bump {
	struct Bump *next;
	char *cursor, *limit, b[];
};

static void bump_free(struct Bump *head) {
	while (head) {
		struct Bump *next = head->next;
		free(head);
		head = next;
	}
}

/** Copies the Emacs string to make its contents accessible. */
static struct Str copy_emacs_string(emacs_env *env, struct Bump **bump, emacs_value value) {
	char *buf = NULL;
	ptrdiff_t origlen, len;
	if (*bump) {
		// Opportunistically try to copy into remaining space
		buf = (*bump)->cursor;
		len = origlen = (*bump)->limit - (*bump)->cursor;
	}
	// Determine the size of the string (including null-terminator)
	if (env->copy_string_contents(env, value, buf, &len)) {
		if (buf) goto success;
	} else {
		if (!buf || len == origlen) return (struct Str) { 0 };
		env->non_local_exit_clear(env);
	}

	size_t capacity = *bump ? 2 * ((*bump)->limit - (*bump)->b) : 2048;
	if (capacity < (size_t) len) capacity = len + alignof(uint64_t) - 1;
	struct Bump *new;
	if (!(new = malloc(sizeof *new + capacity))) return (struct Str) { 0 };
	*new = (struct Bump) { .next = *bump, .cursor = new->b, .limit = new->b + capacity };
	*bump = new;

	env->copy_string_contents(env, value, buf = new->cursor, &len);
success:
	(*bump)->cursor = (char *) (((uintptr_t) (*bump)->cursor + len
			+ alignof(uint64_t) - 1) & ~(alignof(uint64_t) - 1));
	return (struct Str) { buf, len - 1 };
}

struct Candidate {
	emacs_value value;
	union {
		struct Str s;
		int key;
	};
};

static int cmp_candidate(const void *a, const void *b) {
	return ((struct Candidate *) a)->key - ((struct Candidate *) b)->key;
}

struct Batch {
	unsigned len;
	struct Candidate xs[BATCH_SIZE];
};

struct Shared {
	const bool ignore_case;
	const struct Str needle;
	struct Batch *const batches;
	_Atomic ssize_t remaining;
};

static void *worker_routine(void *ptr) {
	struct Shared *shared = ptr;
	struct Str needle = shared->needle;

	ssize_t batch_idx;
	while ((batch_idx = --shared->remaining) >= 0) {
		struct Batch *batch = shared->batches + batch_idx;
		unsigned n = 0;
		for (unsigned i = 0; i < batch->len; ++i) {
			struct Candidate x = batch->xs[i];
			if (!is_match(needle.b, x.s.b, shared->ignore_case)) continue;
			x.key = calc_cost(needle, x.s, shared->ignore_case);
			batch->xs[n++] = x;
		}
		batch->len = n;
	}

	return NULL;
}

/** Module userdata allocated at initialization. */
struct Data {
	unsigned max_workers;
	pthread_t threads[];
};

static emacs_value hotfuzz_filter(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data_ptr) {
	struct Data *data = data_ptr;
	emacs_value fcar = env->intern(env, "car"), fcdr = env->intern(env, "cdr"),
		fcons = env->intern(env, "cons"), nil = env->intern(env, "nil");
	struct Bump *bump = NULL;
	int success = false;
	emacs_value result = nil;

	// Collect all candidates
	struct Batch *batches = NULL;
	size_t batch_idx = 0, capacity;
	for (emacs_value list = args[1]; env->is_not_nil(env, list);
			list = env->funcall(env, fcdr, 1, (emacs_value[]) { list })) {
		if (!batches || (batches[batch_idx].len >= BATCH_SIZE && ++batch_idx >= capacity)) {
			capacity = batches ? 2 * capacity : 1;
			struct Batch *new_batches;
			if (!(new_batches = realloc(batches, capacity * sizeof *batches))) goto err;
			batches = new_batches;
			for (size_t i = batch_idx; i < capacity; ++i) batches[i].len = 0;
		}

		emacs_value value = env->funcall(env, fcar, 1, (emacs_value[]) { list });
		struct Batch *batch = batches + batch_idx;
		struct Candidate *x = batch->xs + batch->len++;
		if (!(x->s = copy_emacs_string(env, &bump, x->value = value)).b) goto err;
	}
	if (!batches) return nil;

	bool ignore_case = env->is_not_nil(env, args[2]);
	struct Str needle = copy_emacs_string(env, &bump, args[0]);
	if (!needle.b) goto err;
	if (ignore_case) strtolower(needle);
	struct Shared shared = {
		.ignore_case = ignore_case,
		.needle = needle,
		.batches = batches,
		.remaining = batch_idx + 1,
	};

	unsigned num_workers = 0;
	for (; num_workers < MIN(data->max_workers, batch_idx + 1); ++num_workers)
		if (pthread_create(data->threads + num_workers, NULL, worker_routine, &shared))
			// Join all workers in order to at least safely free memory
			goto err_join_threads;
	success = true;

err_join_threads:
	// Wait for all worker threads
	for (unsigned i = 0; i < num_workers; ++i) pthread_join(data->threads[i], NULL);
	if (!success) goto err;

	if (env->process_input(env) == emacs_process_input_quit) goto err;

	// Compact all batches
	size_t len = batches[0].len;
	struct Candidate *xs = batches[0].xs;
	for (struct Batch *b = batches + 1; b <= batches + batch_idx; ++b) {
		unsigned n = b->len;
		memmove(xs + len, b->xs, n * sizeof *b->xs);
		len += n;
	}
	qsort(xs, len, sizeof *xs, cmp_candidate); // Sort the completions

	for (size_t i = len; i-- > 0;)
		result = env->funcall(env, fcons, 2, (emacs_value[]) { xs[i].value, result });

err:
	free(batches);
	bump_free(bump);

	if (!success)
		env->non_local_exit_signal(env, env->intern(env, "error"), nil);
	return result;
}

int emacs_module_init(struct emacs_runtime *rt) {
	// Verify compatibility with the Emacs executable loading this module
	if ((size_t) rt->size < sizeof *rt) return 1;
	emacs_env *env = rt->get_environment(rt);
	if ((size_t) env->size < sizeof *env) return 2;

	long max_workers = sysconf(_SC_NPROCESSORS_ONLN);
	struct Data *data;
	if (!(data = malloc(sizeof *data + max_workers * sizeof *data->threads)))
		return 1;
	*data = (struct Data) { max_workers };

	env->funcall(env, env->intern(env, "defalias"), 2, (emacs_value[]) {
			env->intern(env, "hotfuzz--filter-c"),
			env->make_function(env, 3, 3, hotfuzz_filter,
				"Filter and sort CANDIDATES that match STRING.\n"
				"\n"
				"\(fn STRING CANDIDATES IGNORE-CASE)",
				data),
		});
	env->funcall(env, env->intern(env, "provide"), 1,
		(emacs_value[]) { env->intern(env, "hotfuzz-module") });

	return 0;
}
