#include "bettermc.h"
#include "rsort.h"
#include <string.h>
#include <stdlib.h>

#if UINTPTR_MAX == 0xffffffff
#define n_pass_value 4
#elif UINTPTR_MAX == 0xffffffffffffffff
#define n_pass_value 8
#else
#error "failed to detect if this is a 32- or 64-bit system"
#endif



SEXP char_map(SEXP x) {
  SEXP unique;
  SEXP map;

  const int n = LENGTH(x);

  if (n == 0) {
    unique = PROTECT(allocVector(STRSXP, 0));
    map = PROTECT(allocVector(INTSXP, 0));
    goto end;
  }

  const intptr_t *restrict input = (intptr_t *) DATAPTR(x);

#if n_pass_value == 4
  struct uniqueN_data_UINT32_UINT32 * restrict uniqueN_data =
    (struct uniqueN_data_UINT32_UINT32 *) malloc(n * sizeof(struct uniqueN_data_UINT32_UINT32));
  if (uniqueN_data == NULL) {
    error("'malloc' failed to allocate %zu bytes", n * sizeof(struct uniqueN_data_UINT32_UINT32));
  }
#else
  struct uniqueN_data_UINT32_UINT64 * restrict uniqueN_data =
    (struct uniqueN_data_UINT32_UINT64 *) malloc(n * sizeof(struct uniqueN_data_UINT32_UINT64));
  if (uniqueN_data == NULL) {
    error("'malloc' failed to allocate %zu bytes", n * sizeof(struct uniqueN_data_UINT32_UINT64));
  }
#endif

  uint64_t (*restrict hist_value)[n_bucket] = malloc(sizeof(uint64_t[n_pass_value][n_bucket]));
  if (hist_value == NULL) {
    free(uniqueN_data);
    error("'malloc' failed to allocate %zu bytes", sizeof(uint64_t[n_pass_value][n_bucket]));
  }
  memset(hist_value, 0, n_pass_value * n_bucket * sizeof(uint64_t));

  for (uint32_t i = 0; i < n; i++) {
    (uniqueN_data + i)->rank = i;
    (uniqueN_data + i)->value = input[i];

    for (int jj = 0; jj < n_pass_value; jj++) {
      hist_value[jj][(uniqueN_data + i)->value >> jj * shift & mask]++;
    }
  }

  int r = rsort(uniqueN_data, n, NULL, hist_value, VALUE_THEN_RANK);
  free(hist_value);
  if (r != 0) {
    free(uniqueN_data);
#if n_pass_value == 4
    error("'malloc' failed to allocate %zu bytes", n * sizeof(struct uniqueN_data_UINT32_UINT32));
#else
    error("'malloc' failed to allocate %zu bytes", n * sizeof(struct uniqueN_data_UINT32_UINT64));
#endif
  }

  map = PROTECT(allocVector(INTSXP, n));
  int *restrict map_ptr = INTEGER(map);
  int unique_cntr = 1;
  map_ptr[uniqueN_data->rank] = unique_cntr;

  for (int i = 1, ii = 0; i < n; i++) {
    if ((uniqueN_data + i)->value != (uniqueN_data + i - 1)->value) {
      unique_cntr++;
      (uniqueN_data + ii)->rank = i;
      ii = i;
    }
    map_ptr[(uniqueN_data + i)->rank] = unique_cntr;
  }

  unique = PROTECT(allocVector(STRSXP, unique_cntr));
  SET_STRING_ELT(unique, 0, (SEXP) uniqueN_data->value);

  for (int i = 1, ii = 0; i < unique_cntr; i++) {
    ii = (uniqueN_data + ii)->rank;
    SET_STRING_ELT(unique, i, (SEXP) (uniqueN_data + ii)->value);
  }

  free(uniqueN_data);

  end: ;
  SEXP ret = PROTECT(allocVector(VECSXP, 3));
  SET_VECTOR_ELT(ret, 0, unique);
  SET_VECTOR_ELT(ret, 1, map);
  SEXP A = PROTECT(shallow_duplicate(ATTRIB(x)));
  SET_VECTOR_ELT(ret, 2, A);

  UNPROTECT(4);
  return(ret);
}


SEXP char_map_long(SEXP x) {
  SEXP unique;
  SEXP map;

  const R_xlen_t n = XLENGTH(x);

  if (n == 0) {
    unique = PROTECT(allocVector(STRSXP, 0));
    map = PROTECT(allocVector(REALSXP, 0));
    goto end;
  }

  const intptr_t *restrict input = (intptr_t *) DATAPTR(x);

#if n_pass_value == 4
  struct uniqueN_data_UINT64_UINT32 * restrict uniqueN_data =
    (struct uniqueN_data_UINT64_UINT32 *) malloc(n * sizeof(struct uniqueN_data_UINT64_UINT32));
  if (uniqueN_data == NULL) {
    error("'malloc' failed to allocate %zu bytes", n * sizeof(struct uniqueN_data_UINT64_UINT32));
  }
#else
  struct uniqueN_data_UINT64_UINT64 * restrict uniqueN_data =
    (struct uniqueN_data_UINT64_UINT64 *) malloc(n * sizeof(struct uniqueN_data_UINT64_UINT64));
  if (uniqueN_data == NULL) {
    error("'malloc' failed to allocate %zu bytes", n * sizeof(struct uniqueN_data_UINT64_UINT64));
  }
#endif

  uint64_t (*restrict hist_value)[n_bucket] = malloc(sizeof(uint64_t[n_pass_value][n_bucket]));
  if (hist_value == NULL) {
    free(uniqueN_data);
    error("'malloc' failed to allocate %zu bytes", sizeof(uint64_t[n_pass_value][n_bucket]));
  }
  memset(hist_value, 0, n_pass_value * n_bucket * sizeof(uint64_t));

  for (uint64_t i = 0; i < n; i++) {
    (uniqueN_data + i)->rank = i;
    (uniqueN_data + i)->value = input[i];

    for (int jj = 0; jj < n_pass_value; jj++) {
      hist_value[jj][(uniqueN_data + i)->value >> jj * shift & mask]++;
    }
  }

  int r = rsort(uniqueN_data, n, NULL, hist_value, VALUE_THEN_RANK);
  free(hist_value);
  if (r != 0) {
    free(uniqueN_data);
#if n_pass_value == 4
    error("'malloc' failed to allocate %zu bytes", n * sizeof(struct uniqueN_data_UINT64_UINT32));
#else
    error("'malloc' failed to allocate %zu bytes", n * sizeof(struct uniqueN_data_UINT64_UINT64));
#endif
  }

  map = PROTECT(allocVector(REALSXP, n));
  double *restrict map_ptr = REAL(map);
  double unique_cntr = 1;
  map_ptr[uniqueN_data->rank] = unique_cntr;

  for (uint64_t i = 1, ii = 0; i < n; i++) {
    if ((uniqueN_data + i)->value != (uniqueN_data + i - 1)->value) {
      unique_cntr++;
      (uniqueN_data + ii)->rank = i;
      ii = i;
    }
    map_ptr[(uniqueN_data + i)->rank] = unique_cntr;
  }

  unique = PROTECT(allocVector(STRSXP, unique_cntr));
  SET_STRING_ELT(unique, 0, (SEXP) uniqueN_data->value);

  for (uint64_t i = 1, ii = 0; i < unique_cntr; i++) {
    ii = (uniqueN_data + ii)->rank;
    SET_STRING_ELT(unique, i, (SEXP) (uniqueN_data + ii)->value);
  }

  free(uniqueN_data);

  end: ;
  SEXP ret = PROTECT(allocVector(VECSXP, 3));
  SET_VECTOR_ELT(ret, 0, unique);
  SET_VECTOR_ELT(ret, 1, map);
  SET_VECTOR_ELT(ret, 2, PROTECT(shallow_duplicate(ATTRIB(x))));

  UNPROTECT(4);
  return(ret);
}


SEXP set_attr(SEXP x, SEXP attr) {
  ATTRIB(x) = shallow_duplicate(attr);
  SEXP A = getAttrib(x, R_ClassSymbol);
  if (! isNull(A)) OBJECT(x) = 1;

  return x;
}
