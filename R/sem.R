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
#' @name sem
NULL

#' @rdname sem
#' @export
sem_open <- function(name, create = FALSE, overwrite = FALSE, value = 0) {
  .Call(C_semaphore_open, name, create, overwrite, value)
}

#' @rdname sem
#' @export
sem_post <- function(sem) {
  invisible(.Call(C_semaphore_post, sem))
}

#' @rdname sem
#' @export
sem_wait <- function(sem) {
  invisible(.Call(C_semaphore_wait, sem))
}

#' @rdname sem
#' @export
sem_close <- function(sem) {
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
#' @name semv
NULL

#' @rdname semv
#' @export
semv_open <- function(value = 0) {
  .Call(C_semaphorev_open, value)
}

#' @rdname semv
#' @export
semv_post <- function(sid, undo = TRUE) {
  invisible(.Call(C_semaphorev_post, sid, undo))
}

#' @rdname semv
#' @export
semv_wait <- function(sid, undo = TRUE) {
  invisible(.Call(C_semaphorev_wait, sid, undo))
}

#' @rdname semv
#' @export
semv_unlink <- function(sid) {
  invisible(.Call(C_semaphorev_unlink, sid))
}
