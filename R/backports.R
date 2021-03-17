isTRUE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && x
}

isFALSE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}

tryInvokeRestart <- function(r, ...) {
  if (!isRestart(r)) {
    r <- findRestart(r)
  }
  if (is.null(r)) {
    invisible(NULL)
  } else {
    invokeRestart(r, ...)
  }
}
