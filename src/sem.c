#include "bettermc.h"
#include <stdlib.h>
#include <semaphore.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>

SEXP semaphore_open(SEXP n, SEXP create, SEXP overwrite, SEXP value) {
  const char *name = CHAR(STRING_ELT(n, 0));

  sem_t *sem;
  if (asLogical(create)) {
    int oflag = O_CREAT;
    if (!asLogical(overwrite)) {
      oflag |= O_EXCL;
    }
    sem = sem_open(name, oflag, S_IRUSR | S_IWUSR, asInteger(value));
  } else {
    sem = sem_open(name, 0);
  }

  if (sem == SEM_FAILED) {
    error("'sem_open' failed with '%s'", strerror(errno));
  }

  return R_MakeExternalPtr(sem, R_NilValue, R_NilValue);
}

SEXP semaphore_post(SEXP sem) {
  if (sem_post(R_ExternalPtrAddr(sem)) == -1) {
    error("'sem_post' failed with '%s'", strerror(errno));
  }

  return R_NilValue;
}

SEXP semaphore_wait(SEXP sem) {
  if (sem_wait(R_ExternalPtrAddr(sem)) == -1) {
    error("'sem_wait' failed with '%s'", strerror(errno));
  }

  return R_NilValue;
}

SEXP semaphore_close(SEXP sem) {
  if (sem_close(R_ExternalPtrAddr(sem)) == -1) {
    error("'sem_close' failed with '%s'", strerror(errno));
  }

  return R_NilValue;
}

SEXP semaphore_unlink(SEXP n) {
  const char *name = CHAR(STRING_ELT(n, 0));
  if (sem_unlink(name) == -1) {
    error("'sem_unlink' failed with '%s'", strerror(errno));
  }

  return R_NilValue;
}
