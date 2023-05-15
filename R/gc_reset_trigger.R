#' Reset the Trigger Thresholds of the Garbage Collector
#'
#' This function repeatedly triggers full garbage collections until the trigger
#' thresholds no longer decrease.
#'
#' @param print_stats should the memory statistics, as returned by
#'   \code{\link{gc}()}, be printed once at the beginning and once at the end of
#'   this function.
#' @param verbose,reset arguments passed to \code{\link{gc}()}.
#'
#' @details After removing huge objects, the "gc trigger" thresholds will remain
#'   on a high level for a very long time because they are only gradually
#'   adjusted downwards on full collections, which are very rare. Higher
#'   thresholds imply a high memory usage of the R process because collections
#'   are triggered less often. This is a particular problem when using
#'   fork-based parallelization, because the child processes will "inherit the
#'   wasteful attitude" from their parent, such that the whole process family
#'   might end up using unbearably much memory.
#'
#' @note Trigger thresholds can also increase due to calling this function. This
#'   is not a bug but due to the internals of the garbage collector.
#'
#' @return the return value of the final call to \code{\link{gc}()}.
#'
#' @export
gc_reset_trigger <- function(print_stats = TRUE, verbose = getOption("verbose"),
                             reset = TRUE) {
  prev <- gc(verbose = verbose, reset = FALSE, full = FALSE)

  if (print_stats) {
    cat("Memory stats before reset:\n\n")
    print(prev)
    cat("\n\n")
  }

  n <- 0L
  repeat {
    n <- n + 1L
    curr <- gc(verbose = verbose, reset = reset, full = TRUE)
    if (all(curr[, "gc trigger"] >= prev[, "gc trigger"])) break
    prev <- curr
  }

  if (print_stats) {
    cat("Memory stats after reset (", n, " full ",
        ngettext(n, "collection", "collections", domain = NA),
        " needed):\n\n", sep = "")
    print(curr)
    cat("\n\n")

    invisible(curr)
  } else {
    curr
  }
}
