#' Extended try
#'
#' Extended version of \code{\link[base]{try}} with support for tracebacks and
#' crash dumps.
#'
#' @inheritParams base::try
#' @param dump.frames should a crash dump (cf. \code{\link[utils]{dump.frames}})
#'   be created in case of an error? The default "partial" omits the frames up
#'   to the call of \code{etry}. "full" and "no" do the obvious. "full_global"
#'   additionally also includes (a copy of) the global environment (cf.
#'   \code{include.GlobalEnv} argument of \code{\link[utils]{dump.frames}}).
#' @param max.lines for \code{etry}, the maximum number of lines to be
#'   \emph{deparsed} per call. For \code{print}, the maximum number of lines to
#'   be \emph{printed} per call. The default for the latter is unlimited.
#'
#' @return For \code{etry}, the value of the expression if \code{expr} is
#'   evaluated without error, but an invisible object of class
#'   \code{c("etry-error", "try-error")} containing the error message if it
#'   fails. This object has three attributes: (condition) the error condition,
#'   (traceback) the traceback as returned by
#'   \code{\link[base:traceback]{.traceback}}, (dump.frames) the crash dump
#'   which can be examined using \code{\link[utils:debugger]{utils::debugger}}.
#'
#' @section Lifecycle:
#'   \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options:
#'   alt='[Stable]'}}}{\strong{[Stable]}}
#'
#' @importFrom utils limitedLabels
#' @export
etry <- function(expr, silent = FALSE,
                 outFile = getOption("try.outFile", default = stderr()),
                 max.lines = 100L,
                 dump.frames = c("partial", "full", "full_global", "no")) {
  dump.frames <- match.arg(dump.frames)

  TB <- NULL
  DP <- list()
  tryCatch(
    withCallingHandlers(expr, error = function(e) {
      # 4L -> ensure that the actual error is on top of the traceback
      # do.call -> trick R CMD check in R versions where .traceback does not
      # have a max.lines-argument
      if ("max.lines" %in% names(formals(.traceback))) {
        TB <<- do.call(.traceback, list(x = 4L, max.lines = max.lines))
      } else {
        opt_bak <- options(deparse.max.lines = max.lines)
        on.exit(options(opt_bak))
        TB <<- do.call(.traceback, list(x = 4L))
      }

      if (dump.frames != "no") {
        calls <- sys.calls()
        DP <<- sys.frames()
        names(DP) <<- limitedLabels(calls)
        if (dump.frames == "full_global") {
          DP <<- c(.GlobalEnv = as.environment(as.list(.GlobalEnv, all.names = TRUE)), DP)
        }
        # ensure that the actual error is last in the crash dump
        DP <<- DP[-c(length(DP) - 1L, length(DP))]
      }
      attr(DP, "error.message") <<- paste0(conditionMessage(e), "\n\n")
      class(DP) <<- "dump.frames"
    }), error = function(e) {
      if (dump.frames == "partial") {
        # 5L -> ensure that DP starts with the call of etry()
        nc <- length(sys.calls())
        idx2drop <- seq_len(nc - 5L)
      } else if (dump.frames == "no") {
        idx2drop <- seq_along(DP)
      } else {
        idx2drop <- integer()
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
