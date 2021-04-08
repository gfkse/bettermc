#' parallel::mclapply Wrapper for Better Performance, Error Handling, Seeding
#' and UX
#'
#' This wrapper for \code{\link[parallel:mclapply]{parallel::mclapply}} adds the
#' following features: \itemize{ \item reliably detect if a child process failed
#' with a fatal error or if it was killed. \item get tracebacks after non-fatal
#' errors in child processes. \item retry on fatal and non-fatal errors. \item
#' fail early after non-fatal errors in child processes. \item get crash dumps
#' from failed child processes. \item capture output from child processes. \item
#' track warnings, messages and other conditions signaled in the child
#' processes. \item return results from child processes using POSIX shared
#' memory to improve performance. \item compress character vectors in results to
#' improve performance. \item reproducibly seed all function calls. \item
#' display a progress bar.}
#'
#' @inheritParams parallel::mclapply
#' @param mc.set.seed \code{TRUE} or \code{FALSE} are directly handled by
#'   \code{\link[parallel:mclapply]{parallel::mclapply}}. \code{bettermc} also
#'   supports two additional values: \code{NA} (the default) - seed every
#'   invocation of \code{FUN} differently but in a reproducible way based on the
#'   current state of the random number generator in the parent process.
#'   integerish value - call \code{set.seed(mc.set.seed)} in the parent and then
#'   continue as if \code{mc.set.seed} was \code{NA}.
#'
#'   In both (\code{NA}- and integerish-) cases, the state of the random number
#'   generator, i.e. the object \code{.Random.seed} in the global environment,
#'   is restored at the end of the function to what it was when \code{mclapply}
#'   was called. If the random number generator is not yet initialized in the
#'   current session, it is initialized internally (by calling \code{runif(1)})
#'   and the resulting state is what gets restored later. In particular, this
#'   means that the seed supplied as \code{mc.set.seed} does \emph{not} seed the
#'   code following the call to \code{mclapply}. All this ensures that arguments
#'   like \code{mc.cores}, \code{mc.force.fork} etc. can be adjusted without
#'   affecting the state of the RNG outside of \code{mclapply}.
#' @param mc.allow.fatal should fatal errors in child processes make
#'   \code{mclapply} fail (\code{FALSE}, default) or merely trigger a warning
#'   (\code{TRUE})?
#'
#'   \code{TRUE} returns objects of classes \code{c("fatal-error", "try-error")}
#'   for failed invocations. Hence, in contrast to
#'   \code{\link[parallel:mclapply]{parallel::mclapply}}, it is OK for
#'   \code{FUN} to return \code{NULL}.
#'
#'   \code{mc.allow.fatal} can also be \code{NULL}. In this case \code{NULL} is
#'   returned, which corresponds to the behavior of
#'   \code{\link[parallel:mclapply]{parallel::mclapply}}.
#' @param mc.allow.error should non-fatal errors in \code{FUN} make
#'   \code{mclapply} fail (\code{FALSE}, default) or merely trigger a warning
#'   (\code{TRUE})? In the latter case, errors are stored as class
#'   \code{c("etry-error", "try-error")} objects, which contain full tracebacks
#'   and potentially crash dumps (c.f. \code{mc.dump.frames} and
#'   \code{\link{etry}}).
#' @param mc.retry \code{abs(mc.retry)} is the maximum number of retries of
#'   failed applications of \code{FUN} in case of both fatal and non-fatal
#'   errors. This is useful if we expect \code{FUN} to fail either randomly
#'   (e.g. non-convergence of a model) or temporarily (e.g. database
#'   connections). Additionally, if \code{mc.retry <= -1}, the value of
#'   \code{mc.cores} is gradually decreased with each retry to a minimum of 1 (2
#'   if \code{mc.force.fork = TRUE}). This is useful if we expect failures due
#'   to too many parallel processes, e.g. the Linux Out Of Memory Killer
#'   sacrificing some of the child processes.
#'
#'   The environment variable "BMC_RETRY" indicates the current retry. A value
#'   of "0" means first try, a value of "1" first \emph{re}try, etc.
#' @param mc.retry.silent should the messages indicating both fatal and
#'   non-fatal failures during all but the last retry be suppressed
#'   (\code{TRUE}) or not (\code{FALSE}, default)?
#' @param mc.retry.fixed.seed should \code{FUN} for a particular element of
#'   \code{X} always be invoked with the same fixed seed (\code{TRUE}) or should
#'   a different seed be used on each try (\code{FALSE}, default)? Only
#'   effective if \code{mc.set.seed} is \code{NA} or a number.
#' @param mc.fail.early should we try to fail fast after encountering the first
#'   (non-fatal) error in \code{FUN}? Such errors will be recorded as objects of
#'   classes \code{c("fail-early-error", "try-error")}.
#' @param mc.dump.frames should we \code{\link[utils]{dump.frames}} on non-fatal
#'   errors in \code{FUN}? The default "partial" omits the frames (roughly) up
#'   to the call of \code{FUN}. See \code{\link{etry}} for the other options.
#' @param mc.dumpto where to save the result including the dumped frames if
#'   \code{mc.dump.frames != "no" & mc.allow.error == FALSE}? Either the name of
#'   the variable to create in the environment \code{bettermc::crash_dumps} or a
#'   path (prefixed with "file://") where to save the object.
#' @param mc.stdout how should standard output from \code{FUN} be handled?
#'   "capture" captures the output (in the child processes) and prints it in the
#'   parent process after \emph{all} calls of \code{FUN} of the current try (cf.
#'   \code{mc.retry}), such that it can be captured, sinked etc. there. "output"
#'   \emph{immediately} forwards the output to stdout of the parent; it cannot
#'   be captured, sinked etc. there. "ignore" means that the output is not
#'   forwarded in any way to the parent process. For consistency, all of this
#'   also applies if \code{FUN} is called directly from the main process, e.g.
#'   because \code{mc.cores = 1}.
#' @param mc.warnings,mc.messages,mc.conditions how should warnings, messages
#'   and other conditions signaled by \code{FUN} be handled? "signal" records
#'   all warnings/messages/conditions (in the child processes) and signals them
#'   in the master process after \emph{all} calls of \code{FUN} of the current
#'   try (cf. \code{mc.retry}). "stop" converts warnings (only) into non-fatal
#'   errors in the child processes directly. "output" \emph{immediately}
#'   forwards the messages to stderr of the parent; no condition is signaled in
#'   the parent process nor is the output capturable/sinkable. "ignore" means
#'   that the conditions are not forwarded in any way to the parent process.
#'   Options prefixed with "m" additionally try to invoke the
#'   "muffleWarning"/"muffleMessage" restart in the child process. Note that, if
#'   \code{FUN} is called directly from the main process, conditions might be
#'   signaled twice in the main process, depending on these arguments.
#' @param mc.compress.chars should character vectors be compressed using
#'   \code{\link{char_map}} before returning them from the child process? Can
#'   also be the minimum length of character vectors for which to enable
#'   compression. This generally increases performance because (de)serialization
#'   of character vectors is particularly expensive.
#' @param mc.compress.altreps should a character vector be compressed if it is
#'   an ALTREP? The default "if_allocated" only does so if the regular
#'   representation was already created. This was chosen as the default because
#'   in this case is is the regular representation which would be serialized.
#' @param mc.share.vectors should non-character \code{\link[base]{atomic}}
#'   vectors, S3 objects based hereon and factors be returned from the child
#'   processes using POSIX shared memory (cf. \code{\link{copy2shm}})? Can also
#'   be the minimum length of vectors for which to use shared memory. This
#'   generally increases performance because shared memory is a much faster form
#'   of inter process communication compared to pipes and we do not need to
#'   serialize the vectors.
#' @param mc.share.altreps should a non-character vector be returned from the
#'   child process using POSIX shared memory if it is an ALTREP?
#' @param mc.share.copy should the parent process use a vector placed in shared
#'   memory due to \code{mc.share.vectors} directly (\code{FALSE}) or rather a
#'   copy of it (\code{TRUE})? See \code{\link{copy2shm}} for the implications.
#' @param mc.shm.ipc should the results be returned from the child processes
#'   using POSIX shared memory (cf. \code{\link{copy2shm}})?
#' @param mc.force.fork should it be ensured that \code{FUN} is always called in
#'   a forked child process, even if \code{length(X) == 1}? This is useful if we
#'   use forking to protect the main R process from fatal errors, memory
#'   corruption, memory leaks etc. occurring in \code{FUN}. This feature
#'   requires that \code{mc.cores >= 2} and also ensures that the effective
#'   value for \code{mc.cores} never drops to less than 2 as a result of
#'   \code{mc.retry} being negative.
#' @param mc.progress should a progress bar be printed to stderr of the parent
#'   process (package \code{progress} must be installed)?
#'
#' @section POSIX Shared Memory: The shared memory objects created by
#'   \code{mclapply} are named as follows (this may be subject to change):
#'   \code{/bmc_ppid_timestamp_idx_cntr} (e.g.
#'   \code{/bmc_21479_1601366973201_16_10}), with \describe{\item{ppid}{the
#'   process id of the parent process.}\item{timestamp}{the time at which
#'   \code{mclapply} was invoked (in milliseconds since epoch; on macOS: seconds
#'   since epoch, due to its 31-character limit w.r.t. POSIX
#'   names).}\item{idx}{the index of the current element of \code{X}
#'   (1-based).}\item{cntr}{an internal counter (1-based) referring to all the
#'   objects created due to \code{mc.share.vectors} for the current value of
#'   \code{X}; a value of \code{0} is used for the object created due to
#'   \code{mc.shm.ipc}.}}
#'
#'   \code{bettermc::mclapply} does not err if copying data to shared memory
#'   fails. It will rather only print a message and return results the usual
#'   way.
#'
#'   POSIX shared memory has (at least) kernel persistence, i.e. it is not
#'   automatically freed due to process termination, except if the object is/was
#'   unlinked. \code{bettermc} tries hard to not leave any byte behind, but it
#'   could happen that unlinking is incomplete if the parent process is
#'   terminated while \code{bettermc::mclapply} is running.
#'
#'   On Linux you can generally inspect the (not-unlinked) objects currently
#'   stored in shared memory by listing the files under \emph{/dev/shm}.
#'
#' @section (Linux) Size of POSIX Shared Memory: On Linux, POSIX shared memory
#'   is implemented using a
#'   \emph{\href{https://man7.org/linux/man-pages/man5/tmpfs.5.html}{tmpfs}}
#'   typically mounted under \code{/dev/shm}. If not changed by the
#'   distribution, the default size of it is 50\% of physical RAM. It can be
#'   changed (temporarily) by remounting it with a different value for the
#'   \emph{size} option, e.g. \code{mount -o "remount,size=90\%" /dev/shm}.
#'
#' @section (Linux) POSIX Shared Memory and Transparent Hugepage Support: When
#'   allocating a shared memory object of at least
#'   \code{getOption("bettermc.hugepage_limit", 104857600)} bytes of size
#'   (default is 100 MiB), we use
#'   \href{https://man7.org/linux/man-pages/man2/madvise.2.html}{\code{madvise}}\code{(...,
#'    MADV_HUGEPAGE)} to request the allocation of
#'   \href{https://www.kernel.org/doc/Documentation/vm/transhuge.txt}{(transparent)
#'    huge pages}. For this to have any effect, the
#'   \emph{\href{https://man7.org/linux/man-pages/man5/tmpfs.5.html}{tmpfs}}
#'   used to implement POSIX shared memory on Linux (typically mounted under
#'   \code{/dev/shm}) must be (re)mounted with option \emph{huge=advise}, i.e.
#'   \code{mount -o remount,huge=advise /dev/shm}. (The default is
#'   \code{huge=never}, but this might be distribution-specific.)
#'
#' @seealso \code{\link{copy2shm}}, \code{\link{char_map}},
#'   \code{\link[parallel:mclapply]{parallel::mclapply}}
#'
#' @section Windows Support: On Windows, otherwise valid values for various
#'   arguments are silently replaced as follows:
#' \preformatted{  mc.cores <- 1L
#'   mc.share.vectors <- Inf
#'   mc.shm.ipc <- FALSE
#'   mc.force.fork <- FALSE
#'   mc.progress <- FALSE
#'   if (mc.stdout == "output") mc.stdout <- "ignore"
#'   if (mc.warnings == "output") mc.warnings <- "ignore"
#'   if (mc.messages == "output") mc.messages <- "ignore"}
#'
#'   \bold{Note:} \code{\link[parallel:mclapply]{parallel::mclapply}} demands
#'   \code{mc.cores} to be exactly 1 on Windows; \code{bettermc::mclapply} sets
#'   it to 1 on Windows.
#'
#'   Furthermore, \code{\link[parallel:mclapply]{parallel::mclapply}} ignores
#'   the following arguments on Windows: \code{mc.preschedule, mc.silent,
#'   mc.cleanup, mc.allow.recursive, affinity.list}. For \code{mc.set.seed},
#'   only the values \code{TRUE} and \code{FALSE} are ignored (by
#'   \code{\link[parallel:mclapply]{parallel::mclapply}}); the other values are
#'   handled by \code{bettermc::mclapply} as documented above.
#'
#'
#' @section Lifecycle:
#'   \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options:
#'    alt='[Stable]'}}}{\strong{[Stable]}}
#'
#' @return \code{mclapply} returns a list of the same length as X and named by
#'   X. In case of fatal/non-fatal errors and depending on
#'   \code{mc.allow.fatal}/\code{mc.allow.error}/\code{mc.fail.early}, some of
#'   the elements might inherit from
#'   "fatal-error"/\link[=etry]{"etry-error"}/"fail-early-error" and "try-error"
#'   or be \code{NULL}.
#'
#' @importFrom utils capture.output
#' @export
mclapply <- function(X, FUN, ...,
                     mc.preschedule = TRUE, mc.set.seed = NA,
                     mc.silent = FALSE, mc.cores = getOption("mc.cores", 2L),
                     mc.cleanup = TRUE, mc.allow.recursive = TRUE,
                     affinity.list = NULL,
                     mc.allow.fatal = FALSE, mc.allow.error = FALSE,
                     mc.retry = 0L,
                     mc.retry.silent = FALSE,
                     mc.retry.fixed.seed = FALSE,
                     mc.fail.early = !(mc.allow.error || mc.retry != 0L),
                     mc.dump.frames = c("partial", "full", "full_global", "no"),
                     mc.dumpto = ifelse(interactive(), "last.dump",
                                        "file://last.dump.rds"),
                     mc.stdout = c("capture", "output", "ignore"),
                     mc.warnings = c("m_signal", "signal", "m_output", "output",
                                     "m_ignore", "ignore", "stop"),
                     mc.messages = c("m_signal", "signal", "m_output", "output",
                                     "m_ignore", "ignore"),
                     mc.conditions = c("signal", "ignore"),
                     mc.compress.chars = TRUE,
                     mc.compress.altreps = c("if_allocated", "yes", "no"),
                     mc.share.vectors = getOption("bettermc.use_shm", TRUE),
                     mc.share.altreps = c("no", "yes", "if_allocated"),
                     mc.share.copy = TRUE,
                     mc.shm.ipc = getOption("bettermc.use_shm", TRUE),
                     mc.force.fork = FALSE,
                     mc.progress = interactive()) {

  # as in parallel::mclapply
  if (!is.vector(X) || is.object(X))
    X <- as.list(X)

  if (!length(X)) {
    res <- list()
    names(res) <- names(X)
    return(res)
  }

  checkmate::qassert(mc.set.seed, c("b1", "n1"))

  checkmate::assert_flag(mc.allow.fatal, null.ok = TRUE)
  checkmate::assert_flag(mc.allow.error)
  checkmate::assert_int(mc.retry)
  checkmate::assert_flag(mc.retry.silent)
  checkmate::assert_flag(mc.retry.fixed.seed)
  checkmate::assert_flag(mc.fail.early)
  checkmate::assert_string(mc.dumpto, min.chars = 1L)
  checkmate::assert_flag(mc.shm.ipc)
  checkmate::assert_flag(mc.share.copy)

  mc.dump.frames <- match.arg(mc.dump.frames)
  mc.stdout <- match.arg(mc.stdout)
  mc.warnings <- match.arg(mc.warnings)
  if (mc.muffle_warnings <- grepl("^m_", mc.warnings)) {
    mc.warnings <- sub("^m_", "", mc.warnings)
  }
  mc.messages <- match.arg(mc.messages)
  if (mc.muffle_messages <- grepl("^m_", mc.messages)) {
    mc.messages <- sub("^m_", "", mc.messages)
  }
  mc.conditions <- match.arg(mc.conditions)
  mc.compress.altreps <- match.arg(mc.compress.altreps)
  mc.share.altreps <- match.arg(mc.share.altreps)

  checkmate::qassert(mc.compress.chars, c("B1", "N1[0,]"))
  if (isTRUE(mc.compress.chars)) {
    mc.compress.chars <- 0L
  } else if (isFALSE(mc.compress.chars)) {
    mc.compress.chars <- Inf
  }

  checkmate::qassert(mc.share.vectors, c("B1", "N1[0,]"))
  if (isTRUE(mc.share.vectors)) {
    mc.share.vectors <- 0L
  } else if (isFALSE(mc.share.vectors)) {
    mc.share.vectors <- Inf
  }

  checkmate::assert_flag(mc.force.fork)
  if (mc.force.fork && mc.cores < 2L) {
    stop("'mc.force.fork' requires 'mc.cores' to be at least 2.")
  }

  checkmate::assert_flag(mc.progress)
  if (mc.progress && !requireNamespace("progress", quietly = TRUE)) {
    mc.progress <- FALSE
    message("Please install the progress-package in order to get a progress bar.")
  }

  if (!is.null(affinity.list) && mc.preschedule) {
    # originally signaled in parallel::mclapply, but we muffle it
    warning("'mc.preschedule' must be false if 'affinity.list' is used")
  }

  if (OSTYPE == "windows") {
    mc.cores <- 1L
    mc.share.vectors <- Inf
    mc.shm.ipc <- FALSE
    mc.force.fork <- FALSE
    mc.progress <- FALSE
    if (mc.stdout == "output") mc.stdout <- "ignore"
    if (mc.warnings == "output") mc.warnings <- "ignore"
    if (mc.messages == "output") mc.messages <- "ignore"
  }

  FUN <- force(FUN)

  root_stop <- make_root_stop()
  root_warning <- make_root_warning()

  if (!isTRUE(mc.set.seed) && !isFALSE(mc.set.seed)) {
    rng_state <- get0(".Random.seed", .GlobalEnv, inherits = FALSE)
    if (is.null(rng_state)) {
      stats::runif(1)  # init RNG
      rng_state <- get0(".Random.seed", .GlobalEnv, inherits = FALSE)
    }
    on.exit(assign(".Random.seed", rng_state, .GlobalEnv), add = TRUE)

    if (!is.na(mc.set.seed)) set.seed(mc.set.seed)
    seeds_list <- lapply(seq_len(abs(mc.retry) + 1), function(i) {
      round(stats::runif(length(X), -.Machine$integer.max, .Machine$integer.max))
    })
    if (mc.retry.fixed.seed) {
      seeds_list[-1L] <- seeds_list[1L]
    }
    mc.set.seed <- TRUE
  } else {
    seeds_list <- NULL
    seeds <- NULL
  }

  # closure to convert an index w.r.t. X to an index w.r.t. the original X;
  # X might be different from X_orig on retires;
  # the index w.r.t. X_orig is needed to prefix output/messages/... and to name
  # shm objects
  X_idx2X_orig_idx <- function(i) X_seq[i]

  # define core ----
  # we need to cleanup after each try, hence the core function such that we can
  # use on.exit
  core <- function(tries_left, try_idx) {
    # ppid is used to name POSIX shared memory objects and semaphores
    ppid <- Sys.getpid()

    if (OSTYPE == "macos") {
      # limit the length of the shm_prefix because macOS allows POSIX names only
      # up to 31 chars
      timestamp <- as.character(round(as.numeric(Sys.time())))
    } else if (OSTYPE %in% c("linux", "solaris")) {
      timestamp <- as.character(round(as.numeric(Sys.time()) * 1000))
    } else if (OSTYPE == "windows") {
      timestamp <- "NOT_USED"
    } else {
      stop("unexpected value for OSTYPE: ", OSTYPE)
    }

    shm_prefix <- sprintf("/bmc_%d_%s_", ppid, timestamp)

    # unlink shared memory objects in case of errors
    #
    # the regular order in which the objects are unlinked is
    # [0, N, N - 1, ..., 2, 1] (cf. the comments in shm2vectors())
    #
    # unlink_all_shm() unlinks in increasing order starting at 'start' and
    # stopping at the first non-existing object
    on.exit({
      if (mc.shm.ipc) {
        lapply(seq_along(X), function(i)
          unlink_all_shm(paste0(shm_prefix, X_idx2X_orig_idx(i), "_"), start = 0L))
      }

      # if mc.shm.ipc == TRUE, then we would generally not need to run
      # unlink_all_shm(..., 1L) again but there is the edge case when object 0
      # was already unlinked but some of the objects 1, 2, ... still exist
      if (!is.infinite(mc.share.vectors)) {
        lapply(seq_along(X), function(i)
          unlink_all_shm(paste0(shm_prefix, X_idx2X_orig_idx(i), "_"), start = 1L))
      }
    }, add = TRUE)


    # create dedicated child process for printing progress bar ----
    # - call pb tick method on increment of semaphore sem
    # - catch messages signaled by tick and cat them to stderr via pipe (to
    #   make it work in RStudio)
    if (mc.progress) {
      sem_name <- sprintf("/bmc_%d_%s", ppid, timestamp)
      sem <- sem_open(sem_name, create = TRUE)
      on.exit(sem_close(sem), add = TRUE)
      on.exit(sem_unlink(sem_name), add = TRUE)

      progress_job <- parallel::mcparallel({
        stderr_pipe <- pipe("cat >&2")
        pb <- progress::progress_bar$new(format = "[:bar] [:current/:total] :eta ETA",
                                         total = length(X), force = TRUE, show_after = 0,
                                         clear = TRUE)
        withCallingHandlers(
          pb$tick(0),
          message = function(m) {
            cat(m$message, file = stderr_pipe)
            invokeRestart("muffleMessage")
          })

        for (i in seq_along(X)) {
          sem_wait(sem)
          withCallingHandlers(
            pb$tick(),
            message = function(m) {
              cat(m$message, file = stderr_pipe)
              invokeRestart("muffleMessage")
            })
        }
        close(stderr_pipe)
      })
    }


    # this file is touched on first error in a child
    if (mc.fail.early) {
      error_file <- tempfile("bmc_error_")
      on.exit(unlink(error_file), add = TRUE)
    }


    # define wrapper ----
    # this closure is the FUN which we call using parallel::mclapply below
    # - it does not operate on X but rather on seq_along(X) in order to know which
    #   element is currently being processed (mc.X.idx)
    # - the result is wrapped in list() to differentiate a legitimate NULL from a
    #   fatal error
    warning_from_user_code <- FALSE
    wrapper <- function(mc.X.idx, ...) {
      if (mc.X.idx == 0L) return(NULL)  # this is always due to mc.force.fork

      # update progress bar once we are done
      if (mc.progress) on.exit(sem_post(sem), add = TRUE)

      # fail early if there was already an error in a child
      if (mc.fail.early && file.exists(error_file)) {
        cond <- simpleError("failing early due to another error")
        return(
          structure(conditionMessage(cond),
                    class = c("fail-early-error", "try-error"),
                    condition = cond)
        )
      }

      if (!is.null(seeds)) {
        set.seed(seeds[mc.X.idx])
      }
      X <- X[[mc.X.idx]]

      if (OSTYPE == "linux") {
        stdout_pipe <- pipe(sprintf("sed -u 's|^|%d/%d: |' >&1", try_idx, X_idx2X_orig_idx(mc.X.idx)))
        stderr_pipe <- pipe(sprintf("sed -u 's|^|%d/%d: |' >&2", try_idx, X_idx2X_orig_idx(mc.X.idx)))
      } else if (OSTYPE %in% c("macos", "solaris")) {
        stdout_pipe <- pipe(sprintf("sed 's|^|%d/%d: |' >&1", try_idx, X_idx2X_orig_idx(mc.X.idx)))
        stderr_pipe <- pipe(sprintf("sed 's|^|%d/%d: |' >&2", try_idx, X_idx2X_orig_idx(mc.X.idx)))
      } else if (OSTYPE == "windows") {
        stdout_pipe <- NULL
        stderr_pipe <- NULL
      } else {
        root_stop("unexpected value for OSTYPE: ", OSTYPE)
      }


      # make output work in RStudio
      if (mc.stdout == "output") {
        sink(stdout_pipe)
        on.exit(sink(), add = TRUE)
      }

      if (OSTYPE != "windows") {
        on.exit(close(stdout_pipe), add = TRUE)
        on.exit(close(stderr_pipe), add = TRUE)
      }

      shm_prefix <- paste0(shm_prefix, X_idx2X_orig_idx(mc.X.idx), "_")

      warnings <- list()
      if (mc.warnings == "signal") {
        whandler <- function(w) {
          warnings <<- c(warnings, list(w))
          if (mc.muffle_warnings) {
            tryInvokeRestart("muffleWarning")
          }
          warning_from_user_code <<- TRUE
        }
      } else if (mc.warnings == "output") {
        whandler <- function(w) {
          cat(capture.output(print(w)), "\n", file = stderr_pipe)
          if (mc.muffle_warnings) {
            tryInvokeRestart("muffleWarning")
          }
          warning_from_user_code <<- TRUE
        }

      } else if (mc.warnings == "stop") {
        whandler <- function(w) {
          w$message <- paste0("(converted from warning) ", w$message)
          attr(w, "class") <- c("simpleError", "error", "condition")
          stop(w)
        }
      } else {
        whandler <- function(w) {
          if (mc.muffle_warnings) {
            tryInvokeRestart("muffleWarning")
          }
          warning_from_user_code <<- TRUE
        }
      }

      messages <- list()
      if (mc.messages == "signal") {
        mhandler <- function(m) {
          messages <<- c(messages, list(m))
          if (mc.muffle_messages) {
            tryInvokeRestart("muffleMessage")
          }
        }
      } else if (mc.messages == "output") {
        mhandler <- function(m) {
          cat(capture.output(print(m)), "\n", file = stderr_pipe)
          if (mc.muffle_messages) {
            tryInvokeRestart("muffleMessage")
          }
        }
      } else {
        mhandler <- function(m) {
          if (mc.muffle_messages) {
            tryInvokeRestart("muffleMessage")
          }
        }
      }

      conditions <- list()
      if (mc.conditions == "signal") {
        chandler <- function(cond) {
          if (!inherits(cond, c("error", "warning", "message"))) {
            conditions <<- c(conditions, list(cond))
          }
        }
      } else {
        chandler <- function(cond) NULL
      }

      evar <- Sys.getenv("BMC_RETRY", unset = NA)
      if (is.na(evar)) {
        on.exit(Sys.unsetenv("BMC_RETRY"), add = TRUE)
      } else {
        # maybe a recursive call
        on.exit(Sys.setenv(BMC_RETRY = evar), add = TRUE)
      }
      Sys.setenv(BMC_RETRY = try_idx)

      # evaluate FUN and handle errors (etry), warnings and messages;
      # res is always a one-element list except in case of error when it is an
      # etry-error-object
      if (mc.stdout == "capture") {
        output <- capture.output(
          res <- etry(withCallingHandlers(list(FUN(X, ...)),
                                          warning = whandler,
                                          message = mhandler,
                                          condition = chandler),
                      silent = TRUE,
                      dump.frames = if (tries_left) "no" else mc.dump.frames)
        )
        if (length(output)) attr(res, "bettermc_output") <- output
      } else {
        res <- etry(withCallingHandlers(list(FUN(X, ...)),
                                        warning = whandler,
                                        message = mhandler,
                                        condition = chandler),
                    silent = TRUE,
                    dump.frames = if (tries_left) "no" else mc.dump.frames)
      }


      # make consecutive invocations of this wrapper fail early
      if (mc.fail.early && inherits(res, "etry-error")) file.create(error_file)

      if (length(warnings)) attr(res, "bettermc_warnings") <- warnings
      if (length(messages)) attr(res, "bettermc_messages") <- messages
      if (length(conditions)) attr(res, "bettermc_conditions") <- conditions

      if (!is.infinite(mc.compress.chars)) {
        res <- compress_chars(res, limit = mc.compress.chars,
                              compress_altreps = mc.compress.altreps,
                              class = "bmc_internal_char_map")
      }

      if (!is.infinite(mc.share.vectors)) {
        res <- vectors2shm(res, limit = mc.share.vectors,
                           share_altreps = mc.share.altreps,
                           copy = mc.share.copy,
                           name_prefix = shm_prefix,
                           class = "bmc_internal_shm_obj")
      }

      if (mc.shm.ipc) {
        # if copy2shm fails we deliberately sacrifice the serialized result and
        # let parallel::mclapply serialize it again because returning raw vectors
        # used to be fairly buggy
        # (cf. e.g. https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17779)
        res_serial <- serialize(res, NULL, xdr = FALSE)
        shm_obj <- copy2shm(res_serial, paste0(shm_prefix, 0))

        if (inherits(shm_obj, "shm_obj")) {
          res <- shm_obj
        } else {
          message(shm_obj)
        }
      }

      res
    }


    # apply wrapper ----
    # parallel-apply wrapper to seq_along(X)
    if (mc.force.fork && length(X) == 1L) {
      X_seq <- c(0L, 1L)
      affinity.list <- c(affinity.list, affinity.list)
    } else {
      X_seq <- seq_along(X)
    }
    withCallingHandlers(
      res <- parallel::mclapply(
        X = X_seq, FUN = wrapper, ... = ...,
        mc.preschedule = mc.preschedule, mc.set.seed = mc.set.seed,
        mc.silent = mc.silent, mc.cores = mc.cores,
        mc.cleanup = mc.cleanup, mc.allow.recursive = mc.allow.recursive,
        affinity.list = affinity.list
      ),
      warning = function(w) {
        if (!warning_from_user_code) {
          tryInvokeRestart("muffleWarning")
        }
        warning_from_user_code <<- FALSE
      }
    )
    if (mc.force.fork && length(X) == 1L) {
      res <- res[-1L]
    }

    # process wrapper results ----
    # if there is an error in our wrapper code it will be caught by the
    # try-wrapper of parallel::mclapply; in this case we must always fail
    if (any(wrapper_error <-
            vapply(res, inherits, logical(1L), what = "try-error") &
            !vapply(res, inherits, logical(1L), what = c("etry-error",
                                                         "fail-early-error")))) {
      orig_message <- res[[which(wrapper_error)[1]]]
      msg <- "error in bettermc wrapper code; first original message:\n\n" %+%
        paste0(capture.output(orig_message), collapse = "\n")
      root_stop(msg)
    }

    # number of results affected by fatal error(s)
    mc_fatal <- sum(vapply(res, is.null, logical(1L)))

    if (mc.progress) {
      # ensure that the child process printing the progress bar is unblocked in
      # case of fatal errors (calling sem_post more often than actually required
      # does not harm)
      for (i in seq_len(mc_fatal)) {
        sem_post(sem)
      }
      # suppressWarnings due to https://bugs.r-project.org/bugzilla/show_bug.cgi?id=18078:
      # we don't really mind if the progress_job was erroneously killed, but we
      # don't want to see a warning because of this;
      # if there is a warning signaled then most probably the progress process
      # was killed -> clear the incomplete line on stderr
      withCallingHandlers(parallel::mccollect(progress_job),
                          warning = function(w) {
                            cat("\r", file = stderr())
                            tryInvokeRestart("muffleWarning")
                          })
    }

    if (mc.shm.ipc) {
      res <- lapply(res, function(e) {
        if (inherits(e, "shm_obj")) {
          unserialize(allocate_from_shm(e))
        } else {
          # there was a fatal error, i.e. e is NULL
          # - or -
          # we returned early from the wrapper due to failing early
          # - or -
          # copy2shm failed in the child process
          e
        }
      })
    }

    if (!is.infinite(mc.share.vectors)) {
      res <- shm2vectors(res, class = "bmc_internal_shm_obj")
    }

    if (!is.infinite(mc.compress.chars)) {
      res <- uncompress_chars(res, class = "bmc_internal_char_map")
    }

    if (mc.stdout == "capture") {
      res <- lapply(seq_along(res), function(i) {
        e <- res[[i]]
        if (!is.null(attr(e, "bettermc_output"))) {
          cat(paste0(sprintf("%d/%d: ", try_idx, X_idx2X_orig_idx(i)) ,attr(e, "bettermc_output")), sep = "\n")
          attr(e, "bettermc_output") <- NULL
        }
        e
      })
    }

    if (mc.warnings == "signal") {
      res <- lapply(seq_along(res), function(i) {
        e <- res[[i]]
        if (!is.null(attr(e, "bettermc_warnings"))) {
          lapply(attr(e, "bettermc_warnings"), function(w) {
            w$message <- sprintf("%d/%d: %s", try_idx, X_idx2X_orig_idx(i), w$message)
            warning(w)
          })
          attr(e, "bettermc_warnings") <- NULL
        }
        e
      })
    }

    if (mc.messages == "signal") {
      res <- lapply(seq_along(res), function(i) {
        e <- res[[i]]
        if (!is.null(attr(e, "bettermc_messages"))) {
          lapply(attr(e, "bettermc_messages"), function(m) {
            m$message <- sprintf("%d/%d: %s", try_idx, X_idx2X_orig_idx(i), m$message)
            message(m)
          })
          attr(e, "bettermc_messages") <- NULL
        }
        e
      })
    }

    if (mc.conditions == "signal") {
      res <- lapply(seq_along(res), function(i) {
        e <- res[[i]]
        if (!is.null(attr(e, "bettermc_conditions"))) {
          lapply(attr(e, "bettermc_conditions"), function(cond) {
            cond$message <- sprintf("%d/%d: %s", try_idx, X_idx2X_orig_idx(i), cond$message)
            signalCondition(cond)
          })
          attr(e, "bettermc_conditions") <- NULL
        }
        e
      })
    }

    if (!mc.retry.silent && tries_left && mc_fatal) {
      msg <- try_idx %+% ": at least one scheduled core did not return results;" %\%
        "maybe it was killed (by the Linux Out of Memory Killer ?) or there" %\%
        "was a fatal error in the forked process(es)"
      message(msg)
    }

    if (!mc.retry.silent && tries_left &&
        any(mc_error <- vapply(res, inherits, logical(1L), what = "etry-error"))) {
      orig_message <- res[[which(mc_error)[1]]]
      msg <- try_idx %+% ": error(s) occured during mclapply; first original message:\n\n" %+%
        paste0(capture.output(orig_message), collapse = "\n")
      message(msg)
    }

    res
  }

  # loop over tries calling core ----
  mc.cores_seq <- if (mc.retry >= 1L) {
    rep(mc.cores, mc.retry + 1L)
  } else if (mc.retry <= -1L) {
    if (mc.force.fork) {
      as.integer(seq(mc.cores, 2L, length.out = abs(mc.retry) + 1L))
    } else {
      as.integer(seq(mc.cores, 1L, length.out = abs(mc.retry) + 1L))
    }
  } else {
    mc.cores
  }

  X_seq <- seq_along(X)
  X_orig <- X
  affinity.list_orig <- affinity.list
  res <- vector("list", length(X))
  for (i in seq_along(mc.cores_seq)) {
    mc.cores <- mc.cores_seq[i]
    if (!is.null(seeds_list)) {
      seeds <- seeds_list[[i]][X_seq]
    }
    tries_left <- i < length(mc.cores_seq)

    if (mc.cores == 1L || (length(X) == 1L && !mc.force.fork)) {
      # parallel::mclapply won't fork
      mc.share.vectors <- Inf
      mc.shm.ipc <- FALSE
      mc.compress.chars <- Inf
    }

    res[X_seq] <- core(tries_left, try_idx = i - 1L)
    X_seq <- which(unlist(lapply(res, function(e) is.null(e) || inherits(e, "try-error"))))

    if (length(X_seq) == 0L) break
    X <- X_orig[X_seq]
    affinity.list <- affinity.list_orig[X_seq]
  }

  # remove the list()-wrapper around each (non-error) element & potentially
  # introduce explicit fatal errors;
  # also check for non-fatal and fatal errors
  error_idx <- 0L
  has_fatal_error <- FALSE
  res <- lapply(seq_along(res), function(i) {
    e <- res[[i]]
    if (inherits(e, "try-error")) {
      if (error_idx == 0L) error_idx <<- i
      e
    } else if (is.null(e)) {
      has_fatal_error <<- TRUE
      if (!is.null(mc.allow.fatal)) {
        cond <- simpleError("child process did not return any results")
        structure(conditionMessage(cond),
                  class = c("fatal-error", "try-error"),
                  condition = cond)
      } else {
        e
      }
    } else {
      e[[1L]]
    }
  })

  names(res) <- names(X_orig)

  # create crash dump; do this only here such that res is fully processed, i.e.
  # list wrappers removed, named etc.
  if (error_idx && !mc.allow.error && mc.dump.frames != "no") {
    if (grepl("^file://", mc.dumpto)) {
      file <- gsub("^file://", "", mc.dumpto)
      if (inherits(try(saveRDS(res, file)), "try-error")) {
        message("failed to save crash dump to ", file)
      } else {
        file <- normalizePath(file)
        message("crash dump saved to file'", file, "'; for debugging the first error, use:\n'{last.dump <- readRDS(\"",
                file, "\"); utils::debugger(attr(last.dump[[", error_idx, "]], \"dump.frames\"))}'")
      }
    } else {
      assign(mc.dumpto, res, crash_dumps)
      message("crash dump saved to object '", mc.dumpto, "' in environment 'bettermc::crash_dumps';",
              " for debugging the first error, use:\n'utils::debugger(attr(bettermc::crash_dumps[[\"",
              mc.dumpto, "\"]][[", error_idx, "]], \"dump.frames\"))'")
    }
  }

  # signal both fatal and not-fatal error as either warnings or errors while
  # ensuring that
  # - warnings are signaled before errors
  # - if there are two errors to signal, make one out of it
  #
  # whether we need to signal warnings and/or errors depends on mc.allow.fatal
  # and mc.allow.error
  e_list <- list()
  w_list <- list()

  if (has_fatal_error) {
    msg <- "at least one scheduled core did not return results;" %\%
      "maybe it was killed (by the Linux Out of Memory Killer ?) or there" %\%
      "was a fatal error in the forked process(es)"
    if (!isFALSE(mc.allow.fatal)) {
      w_list <- c(w_list, list(msg))
    } else {
      e_list <- c(e_list, list(msg))
    }
  }

  if (error_idx) {
    orig_message <- res[[error_idx]]
    msg <- "error(s) occured during mclapply; first original message:\n\n" %+%
      paste0(capture.output(orig_message), collapse = "\n")
    if (mc.allow.error) {
      w_list <- c(w_list, list(msg))
    } else {
      e_list <- c(e_list, list(msg))
    }
  }

  # ?options on warning.length: "sets the truncation limit for error and
  # warning messages. A non-negative integer, with allowed values
  # 100...8170, default 1000."
  #
  # we increase this here because a msg might contain a traceback, which is
  # easily longer than 1000
  opt_bk <- options(warning.length = 8170L)
  on.exit(options(opt_bk), add = TRUE)

  lapply(w_list, root_warning)
  if (length(e_list) == 1L) {
    root_stop(e_list[[1L]])
  } else if (length(e_list) == 2L) {
    msg <- paste0(e_list[[1L]], "\n\n--- AND ---\n\n", e_list[[2L]])
    root_stop(msg)
  }

  res
}

#' @rdname mclapply
#' @usage crash_dumps  # environment with crash dumps created by mclapply (cf. mc.dumpto)
#' @format \code{crash_dumps} is an initially empty environment used to store
#'   the return values of \code{mclapply} (see below) including
#'   \link[=etry]{crash dumps} in case of non-fatal errors and if
#'   \code{mc.dump.frames != "no" & mc.allow.error == FALSE}.
#' @export
crash_dumps <- new.env()
