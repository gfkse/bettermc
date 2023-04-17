#include "bettermc.h"

#ifdef __linux__

#include <errno.h>
#include <string.h>
#include <signal.h>
#include <time.h>
#include <unistd.h>

void timeout_handler(union sigval sv) {
  kill(getpid(), sv.sival_int);
}

SEXP set_timeout(SEXP seconds, SEXP clock_type, SEXP signal) {
  clockid_t clockid;
  timer_t timerid;
  struct sigevent sev;
  struct itimerspec its;
  time_t sec = asInteger(seconds);

  if (asInteger(clock_type) == 1) {
    clockid = CLOCK_REALTIME;
  } else if (asInteger(clock_type) == 2) {
    clockid= CLOCK_PROCESS_CPUTIME_ID;
  } else{
    error("invalid value for 'clock_type'");
  }

  sev.sigev_notify = SIGEV_THREAD;
  sev.sigev_value.sival_int = asInteger(signal);
  sev.sigev_notify_function = &timeout_handler;
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

  return R_MakeExternalPtr(timerid, R_NilValue, R_NilValue);
}

SEXP disable_timeout(SEXP timerid) {
  timer_t tid = R_ExternalPtrAddr(timerid);

  if (tid != NULL) {
    if (timer_delete(tid) == -1) {
      error("'timer_delete' failed with '%s'", strerror(errno));
    }

    R_ClearExternalPtr(timerid);
  }

  return R_NilValue;
}

#else

SEXP set_timeout(SEXP seconds, SEXP clock_type, SEXP signal) {
  error("Only supported on Linux.");
}

SEXP disable_timeout(SEXP timerid) {
  error("Only supported on Linux.");
}

#endif
