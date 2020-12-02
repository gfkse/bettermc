#' Extended try
#'
#' Extended version of \code{\link[base]{try}} with support for tracebacks and
#' crash dumps.
#'
#' @inheritParams base::try
#' @param dump.frames should a crash dump (cf. \code{\link[utils]{dump.frames}})
#'   be created in case of an error? The default "partial" omits the frames up
#'   to the call of \code{etry}. "full" and "no" do the obvious.
#' @param max.lines for \code{etry}, the maximum number of lines to be
#'   \emph{deparsed} per call. For \code{print}, the maximum number of lines to
#'   be \emph{printed} per call. The default for the latter is unlimited.
#' @param TB_skip_before how many calls to skip from the traceback and the
#'   \emph{full} crash dump before the call to \code{etry}.
#' @param TB_skip_after how many calls to skip from the traceback and the crash
#'   dump after the call to \code{etry}, i.e. from \code{expr} itself.
#'
#' @return For \code{etry}, the value of the expression if \code{expr} is
#'   evaluated without error, but an invisible object of class
#'   \code{c("etry-error", "try-error")} containing the error message if it
#'   fails. This object has three attributes: (condition) the error condition,
#'   (traceback) the traceback as returned by
#'   \code{\link[base:traceback]{.traceback}}, (dump.frames) the crash dump
#'   which can be examined using \code{\link[utils:debugger]{utils::debugger}}.
#'
#' @importFrom utils limitedLabels
#' @export
etry <- function(expr, silent = FALSE,
                 outFile = getOption("try.outFile", default = stderr()),
                 max.lines = 100L,
                 dump.frames = c("partial", "full", "no"),
                 TB_skip_before = 0L, TB_skip_after = 0L) {
  dump.frames <- match.arg(dump.frames)

  TB <- NULL
  DP <- NULL
  tryCatch(
    withCallingHandlers(expr, error = function(e) {
      if ("max.lines" %in% names(formals(.traceback))) {
        TB <<- .traceback(3L, max.lines = max.lines)
      } else {
        opt_bak <- options(deparse.max.lines = max.lines)
        on.exit(options(opt_bak))
        TB <<- .traceback(3L)
      }

      calls <- sys.calls()
      DP <<- sys.frames()
      names(DP) <<- limitedLabels(calls)
      DP <<- c(.GlobalEnv = as.environment(as.list(.GlobalEnv, all.names = TRUE)), DP)
      DP <<- DP[-c(length(DP) - 1L, length(DP))]
      attr(DP, "error.message") <<- conditionMessage(e)
      class(DP) <<- "dump.frames"
    }), error = function(e) {
      nc <- length(sys.calls())

      idx2drop <- seq(length(TB) - nc - TB_skip_after,
                      length(TB) - nc + 4L + TB_skip_before)
      TB[idx2drop] <<- NULL

      if (dump.frames == "full") {
        idx2drop <- seq(nc - 2L - TB_skip_before, nc + 2L + TB_skip_after)
      } else if (dump.frames == "no") {
        idx2drop <- seq_along(DP)
      } else {
        idx2drop <- seq(1, nc + 2L + TB_skip_after)
      }

      DP[idx2drop] <<- NULL

      ret <- structure(paste0("Error: ", conditionMessage(e)),
                       class = c("etry-error", "try-error"),
                       condition = e,
                       traceback = TB,
                       dump.frames = DP)

      if (!silent && isTRUE(getOption("show.error.messages"))) {
        cat(capture.output(print(ret)), file = outFile, sep = "\n")
      }

      invisible(ret)
    }
  )
}

#' @rdname etry
#'
#' @param x an object of class "etry-error".
#' @param ... further arguments passed to or from other methods.
#'
#' @export
`print.etry-error` <- function(x, max.lines = getOption("traceback.max.lines",
                                                        getOption("deparse.max.lines", -1L)),
                               ...) {
  cat(paste0(x, "\n\nTraceback:\n"))
  n <- length(xx <- attr(x, "traceback"))
  if (n == 0L)
    cat(gettext("No traceback available"), "\n")
  else {
    for (i in 1L:n) {
      xi <- xx[[i]]
      label <- paste0(n - i + 1L, ": ")
      m <- length(xi)
      srcloc <- if (!is.null(srcref <- attr(xi, "srcref"))) {
        srcfile <- attr(srcref, "srcfile")
        paste0(" at ", basename(srcfile$filename), "#",
               srcref[1L])
      }
      if (is.numeric(max.lines) && max.lines > 0L && max.lines < m) {
        xi <- c(xi[seq_len(max.lines)], " ...")
        m <- length(xi)
      } else if (isTRUE(attr(xi, 'truncated'))) {
        xi <- c(xi, " ...")
        m <- length(xi)
      }
      if (!is.null(srcloc)) {
        xi[m] <- paste0(xi[m], srcloc)
      }
      if (m > 1)
        label <- c(label, rep(substr("          ", 1L,
                                     nchar(label, type = "w")), m - 1L))
      cat(paste0(label, xi), sep = "\n")
    }
  }

  if (length(attr(x, "dump.frames"))) cat(paste0("\nCrash dump avilable. Use 'debugger(attr(*, \"dump.frames\"))' for debugging.\n"))

  invisible()
}
