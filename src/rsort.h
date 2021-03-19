/* Radix Sort for an Array of 2-Tuples of Unsigned Integers
 *
 * In every pass, the algorithm places all tuples into one of n_bucket=256
 * buckets depending on the value of the byte currently being processed.
 * It starts with the least-significant byte.
 *
 * order indicates if the tuples should be sorted first by RANK_THEN_VALUE or
 * rather by VALUE_THEN_RANK
 *
 * hist_rank[i][j] must indicate how many of the n tuples will be places in
 * bucket j in pass i when sorting by rank. Equivalently for hist_value.
 *
 * In pass i a tuple with a value of x for rank/value will be placed in bucket:
 * x >> i * shift & mask.
 *
 * If either hist_rank or hist_value is NULL, the tuples will not be sorted by
 * rank or value, respectively.
 *
 */

#ifndef RSORT_H
#define RSORT_H

#include <stdint.h>

#define shift 8
#define n_bucket 256
#define mask 0xFF

#define RANK_THEN_VALUE 0
#define VALUE_THEN_RANK 1

struct uniqueN_data_UINT32_UINT32 {
  uint32_t rank;
  uint32_t value;
};

struct uniqueN_data_UINT32_UINT64 {
  uint32_t rank;
  uint64_t value;
};

struct uniqueN_data_UINT64_UINT32 {
  uint64_t rank;
  uint32_t value;
};

struct uniqueN_data_UINT64_UINT64 {
  uint64_t rank;
  uint64_t value;
};


int rsort_UINT32_UINT32(struct uniqueN_data_UINT32_UINT32 *restrict x,
                        uint64_t n,
                        uint64_t hist_rank[restrict][n_bucket],
                        uint64_t hist_value[restrict][n_bucket],
                        int order);

int rsort_UINT32_UINT64(struct uniqueN_data_UINT32_UINT64 *restrict x,
                        uint64_t n,
                        uint64_t hist_rank[restrict][n_bucket],
                        uint64_t hist_value[restrict][n_bucket],
                        int order);

int rsort_UINT64_UINT32(struct uniqueN_data_UINT64_UINT32 *restrict x,
                        uint64_t n,
                        uint64_t hist_rank[restrict][n_bucket],
                        uint64_t hist_value[restrict][n_bucket],
                        int order);

int rsort_UINT64_UINT64(struct uniqueN_data_UINT64_UINT64 *restrict x,
                        uint64_t n,
                        uint64_t hist_rank[restrict][n_bucket],
                        uint64_t hist_value[restrict][n_bucket],
                        int order);


#define rsort(x, n, hist_rank, hist_value, order) _Generic((x),               \
  struct uniqueN_data_UINT32_UINT32 *: rsort_UINT32_UINT32,                   \
  struct uniqueN_data_UINT32_UINT64 *: rsort_UINT32_UINT64,                   \
  struct uniqueN_data_UINT64_UINT32 *: rsort_UINT64_UINT32,                   \
  struct uniqueN_data_UINT64_UINT64 *: rsort_UINT64_UINT64                    \
)(x, n, hist_rank, hist_value, order)

#endif
