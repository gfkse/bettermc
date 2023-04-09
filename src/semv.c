#include "bettermc.h"

#ifndef _WIN32

#include <sys/types.h>
#include <sys/sem.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>

union semun {
  int                 val;
  struct semid_ds *   buf;
  unsigned short *    array;
#if defined(__linux__)
  struct seminfo *    __buf;
#endif
};

SEXP semaphorev_open(SEXP value) {

  union semun arg;
  arg.val = asInteger(value);

  int semid = semget(IPC_PRIVATE, 1, S_IRUSR | S_IWUSR);
  if (semid == -1) {
    error("'semget' failed with '%s'", strerror(errno));
  }

  if (semctl(semid, 0, SETVAL, arg) == -1) {
    semaphorev_unlink(ScalarInteger(semid));
    error("'semctl' failed with '%s'", strerror(errno));
  }

  return ScalarInteger(semid);
}

SEXP semaphorev_wait(SEXP sid, SEXP undo) {
  int semid = asInteger(sid);

  struct sembuf sops;

  sops.sem_num = 0;
  sops.sem_op = -1;
  sops.sem_flg = asLogical(undo) ? SEM_UNDO : 0;

  while (semop(semid, &sops, 1) == -1) {
    if (errno != EINTR) {
      error("'semop' failed with '%s'", strerror(errno));
    }
    R_CheckUserInterrupt();
  }

  return R_NilValue;
}

SEXP semaphorev_post(SEXP sid, SEXP undo) {
  int semid = asInteger(sid);

  struct sembuf sops;

  sops.sem_num = 0;
  sops.sem_op = 1;
  sops.sem_flg = asLogical(undo) ? SEM_UNDO : 0;

  if (semop(semid, &sops, 1) == -1) {
    error("'semop' failed with '%s'", strerror(errno));
  }

  return R_NilValue;
}

SEXP semaphorev_unlink(SEXP sid) {
  union semun dummy;
  dummy.val = 0;  // ignored; just to silence a clang -Wuninitialized

  int semid = asInteger(sid);

  if (semctl(semid, 0, IPC_RMID, dummy) == -1) {
    error("'semctl' failed with '%s'", strerror(errno));
  }

  return R_NilValue;
}

#else

SEXP semaphorev_open(SEXP value) {
  error("Not supported on Windows.");
}

SEXP semaphorev_wait(SEXP sid, SEXP undo) {
  error("Not supported on Windows.");
}

SEXP semaphorev_post(SEXP sid, SEXP undo) {
  error("Not supported on Windows.");
}

SEXP semaphorev_unlink(SEXP sid) {
  error("Not supported on Windows.");
}

#endif
