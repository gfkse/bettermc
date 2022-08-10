#ifndef BETTERMC_H
#define BETTERMC_H

#define _POSIX_C_SOURCE 200112L

#include "Rinternals.h"

SEXP copy2shm(SEXP, SEXP, SEXP, SEXP);
SEXP allocate_from_shm(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP unlink_all_shm(SEXP, SEXP);


SEXP is_altrep(SEXP);
SEXP is_allocated(SEXP);

SEXP char_map(SEXP);
SEXP char_map_long(SEXP);
SEXP set_attr(SEXP, SEXP);

SEXP semaphore_open(SEXP, SEXP, SEXP, SEXP);
SEXP semaphore_post(SEXP);
SEXP semaphore_wait(SEXP);
SEXP semaphore_close(SEXP);
SEXP semaphore_unlink(SEXP);
SEXP sigterm(SEXP);

SEXP semaphorev_open(SEXP);
SEXP semaphorev_post(SEXP, SEXP);
SEXP semaphorev_wait(SEXP, SEXP);
SEXP semaphorev_unlink(SEXP);

SEXP is_uneval_promise(SEXP, SEXP);
SEXP is_eval_promise_to_missing_arg(SEXP, SEXP);

#endif
