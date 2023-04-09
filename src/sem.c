#define _GNU_SOURCE
#include "bettermc.h"

#ifndef _WIN32

#include <stdlib.h>
#include <semaphore.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include <signal.h>

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
  /* most of the following logic is concerned with allowing user interrupts
   * while waiting
   *
   * the R signal handler for SIGINT seems to have SA_RESTART flag set, such
   * that there is no trivial way to call R_CheckUserInterrupt()
   *
   * hence, we unset the flag before waiting and make sure to restore the
   * previous state before exiting
   *
   * alternative: use sem_timedwait() and check every few seconds for an
   * interrupt
   */
  struct sigaction newsa;
  struct sigaction oldsa;

  // newsa is the current signal action but with the SA_RESTART flag unset
  sigaction(SIGINT, NULL, &newsa);
  newsa.sa_flags &= ~SA_RESTART;

  sigaction(SIGINT, &newsa, &oldsa);  // activate newsa and store backup in oldsa
  while (sem_wait(R_ExternalPtrAddr(sem)) == -1) {
    sigaction(SIGINT, &oldsa, NULL);  // restore oldsa in case of error/interrupt
    if (errno != EINTR) {
      error("'sem_wait' failed with '%s'", strerror(errno));
    }

    R_CheckUserInterrupt();
    sigaction(SIGINT, &newsa, NULL);
  }
  sigaction(SIGINT, &oldsa, NULL);

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

SEXP sigterm(SEXP pid) {
  int p = asInteger(pid);
  int ret = kill(p, SIGTERM);
  return ScalarInteger(ret);
}

#else

SEXP semaphore_open(SEXP n, SEXP create, SEXP overwrite, SEXP value) {
  error("Not supported on Windows.");
}

SEXP semaphore_post(SEXP sem) {
  error("Not supported on Windows.");
}

SEXP semaphore_wait(SEXP sem) {
  error("Not supported on Windows.");
}

SEXP semaphore_close(SEXP sem) {
  error("Not supported on Windows.");
}

SEXP semaphore_unlink(SEXP n) {
  error("Not supported on Windows.");
}

SEXP sigterm(SEXP pid) {
  error("Not supported on Windows.");
}

#endif
