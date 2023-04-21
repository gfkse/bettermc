#define _GNU_SOURCE

#include "bettermc.h"

#ifndef _WIN32

#include <sys/types.h>
#include <sys/sem.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include <semaphore.h>
#include <sys/mman.h>
#include <signal.h>
#include <time.h>

#include <R_ext/libextern.h>
LibExtern Rboolean R_interrupts_suspended;
LibExtern int R_interrupts_pending;
extern void Rf_onintr(void);

union semun {
  int                 val;
  struct semid_ds *   buf;
  unsigned short *    array;
#if defined(__linux__)
  struct seminfo *    __buf;
#endif
};

void unblock_queue(union sigval sv) {
  sem_post(sv.sival_ptr);
}

SEXP prio_queue_create(SEXP ncpu, SEXP nprio, SEXP seconds) {
  int n_prio = asInteger(nprio);

  // System V sem - the actual queue
  unsigned short vals[n_prio + 1];
  vals[0] = asInteger(ncpu);
  for (int i = 0; i < n_prio; i++) {
    vals[i + 1] = 0;
  }

  union semun arg;
  arg.array = vals;

  int semid = semget(IPC_PRIVATE, n_prio + 1, S_IRUSR | S_IWUSR);
  if (semid == -1) {
    error("'semget' failed with '%s'", strerror(errno));
  }

  if (semctl(semid, 0, SETALL, arg) == -1) {
    semaphorev_unlink(ScalarInteger(semid));
    error("'semctl' failed with '%s'", strerror(errno));
  }

#ifdef __linux__
  // POSIX sem - for the initial blocking
  sem_t *sem_p = (sem_t *) mmap(NULL, sizeof(sem_t), PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0);
  if (sem_p == MAP_FAILED) {
    error("'mmap' failed with '%s'", strerror(errno));
  }

  if (sem_init(sem_p, 1, asInteger(seconds) > 0 ? 0 : 1) == -1) {
    error("'sem_init' failed with '%s'", strerror(errno));
  }

  // timer - for the unblocking
  clockid_t clockid;
  timer_t timerid;
  struct sigevent sev;
  struct itimerspec its;
  time_t sec = asInteger(seconds);

  clockid = CLOCK_REALTIME;

  sev.sigev_notify = SIGEV_THREAD;
  sev.sigev_value.sival_ptr = sem_p;
  sev.sigev_notify_function = &unblock_queue;
  sev.sigev_notify_attributes = NULL;

  if (timer_create(clockid, &sev, &timerid) == -1) {
    error("'timer_create' failed with '%s'", strerror(errno));
  }

  its.it_interval.tv_nsec = 0;
  its.it_interval.tv_sec = 0;
  its.it_value.tv_nsec = 0;
  its.it_value.tv_sec = sec;

  if (timer_settime(timerid, 0, &its, NULL) == -1) {
    timer_delete(timerid);
    error("'timer_settime' failed with '%s'", strerror(errno));
  }
#else
  void *sem_p = NULL;
  void *timerid = NULL;
#endif

  SEXP ret = PROTECT(allocVector(VECSXP, 3));
  SET_VECTOR_ELT(ret, 0, PROTECT(ScalarInteger(semid)));
  SET_VECTOR_ELT(ret, 1, PROTECT(R_MakeExternalPtr(sem_p, R_NilValue, R_NilValue)));
  SET_VECTOR_ELT(ret, 2, PROTECT(R_MakeExternalPtr(timerid, R_NilValue, R_NilValue)));

  UNPROTECT(4);
  return ret;
}

SEXP prio_queue_insert(SEXP sid, SEXP sem, SEXP prio) {
  int semid = asInteger(sid);
  int priority = asInteger(prio);

  Rboolean __oldsusp__ = R_interrupts_suspended;

  // insert into queue
  struct sembuf sop;

  sop.sem_num = priority;
  sop.sem_op = 1;
  sop.sem_flg = SEM_UNDO;

  if (semop(semid, &sop, 1) == -1) {
    error("'semop' failed with '%s'", strerror(errno));
  }

#ifdef __linux__
  // wait for queue to be unblocked
  int sem_val;
  struct timespec ts;
  ts.tv_sec = 1;
  ts.tv_nsec = 0;

  for (;;) {
    sem_t *sem_p = R_ExternalPtrAddr(sem);
    if (sem_getvalue(sem_p, &sem_val) == -1) {
      error("'sem_getvalue' failed with '%s'", strerror(errno));
    }
    if (sem_val) break;

    nanosleep(&ts, NULL);

    R_interrupts_suspended = TRUE;
    R_CheckUserInterrupt();
    R_interrupts_suspended = __oldsusp__;

    if (R_interrupts_pending && ! R_interrupts_suspended) {
      sop.sem_op = -1;

      // this should never block and hence fail with EINTR
      if (semop(semid, &sop, 1) == -1) {
        error("'semop' failed with '%s'", strerror(errno));
      }

      Rf_onintr();
    }
  }
#endif

  // schedule according to priority
  struct sembuf sops[priority + 1];

  sops[0].sem_num = 0;
  sops[0].sem_op = -1;
  sops[0].sem_flg = SEM_UNDO;

  for (int i = 1; i < priority; i++) {
    sops[i].sem_num = i;
    sops[i].sem_op = 0;
    sops[i].sem_flg = 0;  // do not remove this!
  }

  sops[priority].sem_num = priority;
  sops[priority].sem_op = -1;
  sops[priority].sem_flg = SEM_UNDO;

  while (semop(semid, sops, priority + 1) == -1) {
    if (errno != EINTR) {
      error("'semop' failed with '%s'", strerror(errno));
    }

    R_interrupts_suspended = TRUE;
    R_CheckUserInterrupt();
    R_interrupts_suspended = __oldsusp__;

    if (R_interrupts_pending && ! R_interrupts_suspended) {
      sop.sem_op = -1;

      // this should never block and hence fail with EINTR
      if (semop(semid, &sop, 1) == -1) {
        error("'semop' failed with '%s'", strerror(errno));
      }

      Rf_onintr();
    }
  }

  return R_NilValue;
}

SEXP prio_queue_release(SEXP sid) {
  int semid = asInteger(sid);

  struct sembuf sops;

  sops.sem_num = 0;
  sops.sem_op = 1;
  sops.sem_flg = SEM_UNDO;

  if (semop(semid, &sops, 1) == -1) {
    error("'semop' failed with '%s'", strerror(errno));
  }

  return R_NilValue;
}

SEXP prio_queue_destroy(SEXP sid, SEXP sem, SEXP timerid) {
  semaphorev_unlink(sid);

#ifdef __linux__
  sem_t *sem_p = R_ExternalPtrAddr(sem);
  if (sem_p != NULL) {
    if (sem_destroy(sem_p) == -1) {
      error("'sem_destroy' failed with '%s'", strerror(errno));
    }
    if (munmap(sem_p, sizeof(sem_t)) == -1) {
      error("'munmap' failed with '%s'", strerror(errno));
    }

    R_ClearExternalPtr(sem);
  }


  timer_t tid = R_ExternalPtrAddr(timerid);
  if (tid != NULL) {
    if (timer_delete(tid) == -1) {
      error("'timer_delete' failed with '%s'", strerror(errno));
    }

    R_ClearExternalPtr(timerid);
  }
#endif

  return R_NilValue;
}

#else

SEXP prio_queue_create(SEXP ncpu, SEXP nprio, SEXP seconds) {
  error("Not supported on Windows.");
}

SEXP prio_queue_insert(SEXP sid, SEXP sem, SEXP prio) {
  error("Not supported on Windows.");
}

SEXP prio_queue_release(SEXP sid) {
  error("Not supported on Windows.");
}

SEXP prio_queue_destroy(SEXP sid, SEXP sem, SEXP timerid) {
  error("Not supported on Windows.");
}

#endif
