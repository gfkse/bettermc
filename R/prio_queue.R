#' Priority Queues
#'
#' Create and destroy priority queues to be passed to \code{\link{mclapply}} as
#' argument \code{mc.prio.queue}. \code{prio_queue_insert} and
#' \code{prio_queue_release} are used internally by \code{\link{mclapply}} to
#' claim and release CPU units. See the examples for more details.
#'
#' @param ncpu the CPU limit to be imposed by the queue.
#' @param nprio the number priority levels to be supported by the queue.
#' @param seconds2block the number of seconds to block the queue after its
#'   creation. This can be used to ensure that the queue is properly filled
#'   before scheduling the first processes according to their priority. Only
#'   supported on Linux, i.e. \code{seconds2block} must be 0 on macOS (and
#'   Solaris).
#'
#' @note A priority queue should be destroyed explicitly after use to release
#'   the associated resources by calling \code{prio_queue_destroy}. It is
#'   undefined behavior to destroy a queue which is still in use or to call
#'   \code{prio_queue_destroy} multiple times on the same queue.
#'
#'   The number of calls in a process to \code{prio_queue_release} must not
#'   exceed the number of previous call to \code{prio_queue_insert}. Otherwise
#'   undefined behavior results. Calls to \code{prio_queue_release} might be
#'   omitted. In this, case release of previously claimed but not yet released
#'   CPU units happens on process termination.
#'
#' @details The core of the queue is implemented using a set of \code{nprio + 1}
#'   System V semaphores. One semaphore holds the currently available CPU units,
#'   while the others are used to record the queue lengths for the different
#'   priorities. Note that different operating systems impose different limits
#'   w.r.t. System V semaphores. In particular, SEMVMX (the maximum value for a
#'   semaphore) hence limits both \code{ncpu} and the number of processes which
#'   can be queued per priority. On Linux, SEMVMX is 32,767.
#'
#'   The initial blocking is implemented using an unnamed POSIX semaphore and a
#'   POSIX timer, which macOS both lacks support for.
#'
#'
#'
#' @examples
#' \donttest{
#' # interactive() just to make R CMD check on CRAN skip checking this example
#' # because it creates more processes than allowed
#' if (interactive() & bettermc:::OSTYPE != "windows") {
#'   x <- 1:4
#'   y <- 1:5
#'
#'   ncpu <- 4
#'   nprio <- 3
#'   seconds2block = if (bettermc:::OSTYPE == "linux") 1 else 0
#'
#'   pq <- bettermc::prio_queue_create(ncpu = ncpu, nprio = nprio,
#'                                     seconds2block = seconds2block)
#'
#'   res <- bettermc::mclapply(x, function(i) {
#'     # determine priorities of child processes to be created
#'     priorities <- sample(nprio, length(y), replace = TRUE)
#'
#'     bettermc::mclapply(y, function(j) {
#'       # do some work
#'       cat(sprintf("(%s) priority: %d (pid :%d)\n",
#'                   as.character(Sys.time()), priorities[j], Sys.getpid()))
#'       Sys.sleep(1)
#'     },
#'     mc.cores = length(y), mc.prio.queue = pq, mc.priority = priorities,
#'     mc.stdout = "output", mc.progress = FALSE)
#'   },
#'   mc.cores = length(x), mc.progress = FALSE)
#'
#'   bettermc::prio_queue_destroy(pq)
#'
#'   # the above is roughly equivalent to the following more manual approach
#'   pq <- bettermc::prio_queue_create(ncpu = ncpu, nprio = nprio,
#'                                     seconds2block = seconds2block)
#'
#'   res <- bettermc::mclapply(x, function(i) {
#'     # determine priorities of child processes to be created
#'     priorities <- sample(nprio, length(y), replace = TRUE)
#'
#'     bettermc::mclapply(y, function(j) {
#'       bettermc::prio_queue_insert(pq, priorities[j])  # wait for free CPU with priority
#'       on.exit(bettermc::prio_queue_release(pq))  # release CPU when done
#'
#'       # do some work
#'       cat(sprintf("(%s) priority: %d (pid :%d)\n",
#'                   as.character(Sys.time()), priorities[j], Sys.getpid()))
#'       Sys.sleep(1)
#'     },
#'     mc.cores = length(y),
#'     mc.stdout = "output", mc.progress = FALSE)
#'   },
#'   mc.cores = length(x), mc.progress = FALSE)
#'
#'   bettermc::prio_queue_destroy(pq)
#' }
#' }
#'
#' @section Windows Support: Not supported on Windows.
#'
#' @section Lifecycle:
#'   \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options:
#'   alt='[Experimental]'}}}{\strong{[Experimental]}}
#'
#' @export
prio_queue_create <- function(ncpu = parallel::detectCores(), nprio = 1L,
                              seconds2block = 0L) {
  ncpu <- as.integer(ncpu)
  checkmate::assert_number(ncpu, lower = 1L)

  nprio <- as.integer(nprio)
  checkmate::assert_number(nprio, lower = 1L)

  seconds2block <- as.integer(seconds2block)
  checkmate::assert_number(seconds2block, lower = 0L)

  if ((OSTYPE == "macos" | OSTYPE == "solaris") & seconds2block > 0) {
    stop("'seconds2block' must be 0 on macOS and Solaris")
  }

  structure(
    .Call(C_prio_queue_create,
          ncpu = ncpu,
          nprio = nprio,
          seconds = seconds2block),
    names = c("sid", "sem", "timerid"),
    class = "bettermc_prio_queue"
  )
}

#' @rdname prio_queue_create
#'
#' @param pq an object as returned by \code{prio_queue_create}.
#' @param prio the priority of the current process, i.e. an integer between 1
#'   and \code{nprio}. Lower values indicate higher priority.
#'
#' @export
prio_queue_insert <- function(pq, prio) {
  checkmate::assert_class(pq, "bettermc_prio_queue")

  prio <- as.integer(prio)
  checkmate::assert_number(prio, lower = 1L)

  invisible(
    .Call(C_prio_queue_insert, sid = pq[["sid"]], sem = pq[["sem"]], prio = prio)
  )
}

#' @rdname prio_queue_create
#' @export
prio_queue_release <- function(pq) {
  checkmate::assert_class(pq, "bettermc_prio_queue")

  invisible(
    .Call(C_prio_queue_release, sid = pq[["sid"]])
  )
}

#' @rdname prio_queue_create
#' @export
prio_queue_destroy <- function(pq) {
  checkmate::assert_class(pq, "bettermc_prio_queue")

  invisible(
    .Call(C_prio_queue_destroy, sid = pq[["sid"]], sem = pq[["sem"]], timerid = pq[["timerid"]])
  )
}
