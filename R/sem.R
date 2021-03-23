#' Named POSIX Semaphores
#'
#' @param name the name of the semaphore. Consult \code{man sem_overview} for
#'   what makes a valid name.
#' @param create should the semaphore be created if it currently does not exist?
#' @param overwrite if \code{create == TRUE}, should we overwrite an already
#'   existing semaphore with the name (\code{TRUE}) or rather fail
#'   (\code{FALSE}).
#' @param value the initial value of the semaphore (\code{>= 0}).
#' @param sem an object as returned by \code{sem_open}.
#'
#' @section Lifecycle:
#'   \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options:
#'   alt='[Experimental]'}}}{\strong{[Experimental]}}
#'
#' @return For \code{sem_open}, an object of class "sem", which is an external
#'   pointer to the POSIX semaphore. All other functions return \code{NULL}
#'   invisibly and are called for their side effects.
#'
#' @name sem
NULL

#' @rdname sem
#' @export
sem_open <- function(name, create = FALSE, overwrite = FALSE, value = 0) {
  structure(
    .Call(C_semaphore_open, name, create, overwrite, value),
    class = "sem"
  )
}

#' @rdname sem
#' @export
sem_post <- function(sem) {
  stopifnot(inherits(sem, "sem"))
  invisible(.Call(C_semaphore_post, sem))
}

#' @rdname sem
#' @export
sem_wait <- function(sem) {
  stopifnot(inherits(sem, "sem"))
  invisible(.Call(C_semaphore_wait, sem))
}

#' @rdname sem
#' @export
sem_close <- function(sem) {
  stopifnot(inherits(sem, "sem"))
  invisible(.Call(C_semaphore_close, sem))
}

#' @rdname sem
#' @export
sem_unlink <- function(name) {
  invisible(.Call(C_semaphore_unlink, name))
}

#' POSIX-style System V Semaphores
#'
#' Mimic the POSIX semaphore API with System V semaphores.
#'
#' @param value the initial value of the semaphore to create (\code{>= 0}).
#' @param sid the semaphore id as returned by \code{semv_open}.
#' @param undo should the operations (decrement/increment) on the semaphore be
#'   undone on process termination. This feature is probably the main reason to
#'   prefer System V semaphores to POSIX ones.
#'
#' @section Lifecycle:
#'   \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options:
#'   alt='[Experimental]'}}}{\strong{[Experimental]}}
#'
#' @return For \code{semv_open}, an object of class "semv", which is an integer
#'   referring to the System V semaphore. All other functions return \code{NULL}
#'   invisibly and are called for their side effects.
#'
#' @name semv
NULL

#' @rdname semv
#' @export
semv_open <- function(value = 0) {
  structure(
    .Call(C_semaphorev_open, value),
    class = "semv"
  )
}

#' @rdname semv
#' @export
semv_post <- function(sid, undo = TRUE) {
  stopifnot(inherits(sid, "semv"))
  invisible(.Call(C_semaphorev_post, sid, undo))
}

#' @rdname semv
#' @export
semv_wait <- function(sid, undo = TRUE) {
  stopifnot(inherits(sid, "semv"))
  invisible(.Call(C_semaphorev_wait, sid, undo))
}

#' @rdname semv
#' @export
semv_unlink <- function(sid) {
  stopifnot(inherits(sid, "semv"))
  invisible(.Call(C_semaphorev_unlink, sid))
}
