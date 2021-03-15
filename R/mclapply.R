#'parallel::mclapply Wrapper for Better Performance, Error Handling and UX
#'
#'This wrapper for \code{\link[parallel:mclapply]{parallel::mclapply}} adds the
#'following features: \itemize{ \item reliably detect if a child process failed
#'with a fatal error or if it was killed. \item get tracebacks after non-fatal
#'errors in child processes. \item fail early after non-fatal errors in child
#'processes. \item get crash dumps from failed child processes. \item capture
#'output from child processes. \item track warnings and messages signaled in the
#'child processes. \item return results from child processes using POSIX shared
#'memory to improve performance. \item compress character vectors in results to
#'improve performance. \item display a progress bar.}
#'
#'@inheritParams parallel::mclapply
#'@param mc.allow.fatal should fatal errors in child processes make
#'  \code{mclapply} fail (default) or merely trigger a warning?
#'@param mc.allow.error should non-fatal errors in child processes make
#'  \code{mclapply} fail (default) or merely trigger a warning?
#'@param mc.fail.early should we try to fail fast after encountering the first
#'  (non-fatal) error in a child process?
#'@param mc.dump.frames should we \code{\link[utils]{dump.frames}} on non-fatal
#'  errors in child processes. The default "partial" omits the frames up to the
#'  call of \code{mcparallel} in the master processes. "full" and "no" do the
#'  obvious.
#'@param mc.dumpto where to save the result including the dumped frames if
#'  \code{mc.dump.frames != "no" & mc.allow.error == FALSE}? Either the name of
#'  the variable to create in the global environment or a path (prefixed with
#'  "file://") where to save the object.
#'@param mc.stdout how should standard output in the child processes be handled?
#'  "capture" captures the output in the child processes and prints it in the
#'  parent process such that it can be captured, sinked etc. there. "output"
#'  directly forwards the output to stdout of the parent; it cannot be captured,
#'  sinked etc. there.
#'@param mc.warnings,mc.messages,mc.conditions how should warnings, messages and
#'  other conditions in the child processes be handled? "signal" records all
#'  warnings/messages/conditions in the child processes and signals them in the
#'  master process. "stop" converts warnings (only) into non-fatal errors in the
#'  child processes directly. "output" directly forwards the messages to stderr
#'  of the parent; no condition is signalled in the parent process nor is the
#'  output capturable/sinkable. "ignore" means that the conditions are not
#'  forwarded in any way to the parent process. Options prefixed with "m"
#'  additionally try to invoke the "muffleWarning"/"muffleMessage" restart in
#'  the child process.
#'@param mc.compress.chars should character vectors be compressed using
#'  \code{\link{char_map}}? Can also be the minimum length of character vectors
#'  for which to enable compression. This generally increases performance
#'  because (de)serialization of character vectors is particularly expensive.
#'@param mc.compress.altreps should a character vector be compressed if it is an
#'  ALTREP? The default "if_allocated" only does so if the regular
#'  representation was already created. This was chosen as the default because
#'  in this case is is the regular representation which would be serialized.
#'@param mc.share.vectors should non-character \code{\link[base]{atomic}}
#'  vectors, S3 objects based hereon and factors be returned from the child
#'  processes using POSIX shared memory (cf. \code{\link{copy2shm}})? Can also
#'  be the minimum length of vectors for which to use shared memory. This
#'  generally increases performance because shared memory is a much faster form
#'  of inter process communication compared to pipes and we do not need to
#'  serialize the vectors. Note that there is no guarantee that \code{mclapply}
#'  really uses shared memory even if requested: if \code{\link{copy2shm}}
#'  fails, the vector will be returned the usual way.
#'@param mc.share.altreps should a non-character vector be returned from the
#'  child process using POSIX shared memory if it is an ALTREP?
#'@param mc.share.copy should the parent process use a vector placed in shared
#'  memory due to \code{mc.share.vectors} directly (\code{FALSE}) or rather a
#'  copy of it (\code{TRUE})? See \code{\link{copy2shm}} for the implications.
#'@param mc.shm.ipc should the results be returned from the child processes
#'  using POSIX shared memory (cf. \code{\link{copy2shm}})?
#'@param mc.progress should a progress bar be printed to stderr of the parent
#'  process (package \code{progress} must be installed)?
#'
#'@section POSIX Shared Memory: The shared memory objects created by
#'  \code{mclapply} are named as follows (this may be subject to change):
#'  \code{/bmc_ppid_timestamp_idx_cntr} (e.g.
#'  \code{/bmc_21479_1601366973201_16_10}), with \describe{\item{ppid}{the
#'  process id of the parent process.}\item{timestamp}{the time at which
#'  \code{mclapply} was invoked (in milliseconds since epoch).}\item{idx}{the
#'  index of the current element of \code{X} (1-based).}\item{cntr}{an internal
#'  counter (1-based) referring to all the objects created due to
#'  \code{mc.share.vectors} for the current value of \code{X}; a value of
#'  \code{0} is used for the object created due to \code{mc.shm.ipc}.}}
#'
#'  \code{bettermc::mclapply} does not err if copying data to shared memory
#'  fails. It will rather only print a message and return results the usual way.
#'
#'  POSIX shared memory has (at least) kernel persistence, i.e. it is not
#'  automatically freed due to process termination, except if the object is/was
#'  unlinked. \code{bettermc} tries hard to not leave any byte behind, but it
#'  could happen that unlinking is incomplete if the parent process is
#'  terminated while \code{bettermc::mclapply} is running.
#'
#'  On Linux you can generally inspect the (not-unlinked) objects currently
#'  stored in shared memory by listing the files under \emph{/dev/shm}.
#'
#'@section (Linux) Size of POSIX Shared Memory: On Linux, POSIX shared memory is
#'  implemented using a
#'  \emph{\href{https://man7.org/linux/man-pages/man5/tmpfs.5.html}{tmpfs}}
#'  typically mounted under \code{/dev/shm}. If not changed by the distribution,
#'  the default size of it is 50\% of physical RAM. It can be changed
#'  (temporarily) by remounting it with a different value for the \emph{size}
#'  option, e.g. \code{mount -o "remount,size=90\%" /dev/shm}.
#'
#'@section (Linux) POSIX Shared Memory and Transparent Hugepage Support: When
#'  allocating a shared memory object of at least
#'  \code{getOption("bettermc.hugepage_limit", 104857600)} bytes of size
#'  (default is 100 MiB), we use
#'  \href{https://man7.org/linux/man-pages/man2/madvise.2.html}{\code{madvise}}\code{(...,
#'   MADV_HUGEPAGE)} to request the allocation of
#'  \href{https://www.kernel.org/doc/Documentation/vm/transhuge.txt}{(transparent)
#'   huge pages}. For this to have any effect, the
#'  \emph{\href{https://man7.org/linux/man-pages/man5/tmpfs.5.html}{tmpfs}} used
#'  to implement POSIX shared memory on Linux (typically mounted under
#'  \code{/dev/shm}) must be (re)mounted with option \emph{huge=advise}, i.e.
#'  \code{mount -o remount,huge=advise /dev/shm}. (The default is
#'  \code{huge=never}, but this might be distribution-specific.)
#'
#'@seealso \code{\link{copy2shm}}, \code{\link{char_map}},
#'  \code{\link[parallel:mclapply]{parallel::mclapply}}
#'
#'@importFrom utils capture.output
#'@export
mclapply <- function(X, FUN, ...,
                     mc.preschedule = TRUE, mc.set.seed = TRUE,
                     mc.silent = FALSE, mc.cores = getOption("mc.cores", 2L),
                     mc.cleanup = TRUE, mc.allow.recursive = TRUE,
                     affinity.list = NULL,
                     mc.allow.fatal = FALSE, mc.allow.error = FALSE,
                     mc.fail.early = !mc.allow.error,
                     mc.dump.frames = c("partial", "full", "no"),
                     mc.dumpto = ifelse(interactive(), "last.dump",
                                        "file://last.dump.rds"),
                     mc.stdout = c("capture", "output"),
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
                     mc.progress = interactive()) {

  # as in parallel::mclapply
  if (!is.vector(X) || is.object(X))
    X <- as.list(X)

  checkmate::assert_flag(mc.allow.fatal)
  checkmate::assert_flag(mc.allow.error)
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


  checkmate::assert_flag(mc.progress)
  if (mc.progress && !requireNamespace("progress", quietly = TRUE)) {
    mc.progress <- FALSE
    message("Please install the progress-package in order to get a progress bar.")
  }


  FUN <- force(FUN)

  # ppid is used to name POSIX shared memory objects and semaphores
  ppid <- Sys.getpid()

  timestamp <- as.character(round(as.numeric(Sys.time()) * 1000))
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
        unlink_all_shm(paste0(shm_prefix, i, "_"), start = 0L))
    }

    # if mc.shm.ipc == TRUE, then we would generally not need to run
    # unlink_all_shm(..., 1L) again but there is the edge case when object 0
    # was already unlinked but some of the objects 1, 2, ... still exist
    if (!is.infinite(mc.share.vectors)) {
      lapply(seq_along(X), function(i)
        unlink_all_shm(paste0(shm_prefix, i, "_"), start = 1L))
    }
  })


  # create dedicated child process for printing progress bar ----
  # - call pb tick method on increment of semaphore sem
  # - catch messages signalled by tick and cat them to stderr via pipe (to
  #   make it work in RStudio)
  if (mc.progress) {
    sem <- sem_open(paste0("/bettermc_", ppid), create = TRUE)
    on.exit(sem_close(sem), add = TRUE)
    on.exit(sem_unlink(paste0("/bettermc_", ppid)), add = TRUE)

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
    error_file <- tempfile("bettermc_error_")
    on.exit(unlink(error_file), add = TRUE)
  }


  # this closure is the FUN which we call using parallel::mclapply below
  # - it does not operate on X but rather on seq_along(X) in order to know which
  #   element is currently being processed (mc.X.idx)
  # - the result is wrapped in list() to differentiate a legitimate NULL from a
  #   fatal error
  warning_from_user_code <- FALSE
  wrapper <- function(mc.X.idx, ...) {
    # update progress bar once we are done
    if (mc.progress) on.exit(sem_post(sem), add = TRUE)

    # fail early if there was already an error in a child
    if (mc.fail.early && file.exists(error_file))
      return(list(simpleError("failing early due to another error")))

    X <- X[[mc.X.idx]]

    if (OSTYPE == "linux") {
      stdout_pipe <- pipe(sprintf("sed -u 's/^/%5d: /' >&1", mc.X.idx))
      stderr_pipe <- pipe(sprintf("sed -u 's/^/%5d: /' >&2", mc.X.idx))
    } else if (OSTYPE %in% c("macos", "solaris")) {
      stdout_pipe <- pipe(sprintf("sed 's/^/%5d: /' >&1", mc.X.idx))
      stderr_pipe <- pipe(sprintf("sed 's/^/%5d: /' >&2", mc.X.idx))
    } else {
      stop("unexpected value for OSTYPE: ", OSTYPE)
    }


    # make output work in RStudio
    if (mc.stdout == "output") {
      sink(stdout_pipe)
      on.exit(sink(), add = TRUE)
    }

    on.exit(close(stdout_pipe), add = TRUE)
    on.exit(close(stderr_pipe), add = TRUE)

    shm_prefix <- paste0(shm_prefix, mc.X.idx, "_")

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

    # evaluate FUN and handle errors (etry), warnings and messages;
    # res is always a one-element list except in case of error when it is an
    # etry-error-object
    if (mc.stdout == "output") {
      res <- etry(withCallingHandlers(list(FUN(X, ...)),
                                      warning = whandler,
                                      message = mhandler,
                                      condition = chandler),
                  silent = TRUE, dump.frames = mc.dump.frames,
                  TB_skip_before = 14L, TB_skip_after = 2L)
    } else {
      output <- capture.output(
        res <- etry(withCallingHandlers(list(FUN(X, ...)),
                                        warning = whandler,
                                        message = mhandler,
                                        condition = chandler),
                    silent = TRUE, dump.frames = mc.dump.frames,
                    TB_skip_before = 19L, TB_skip_after = 2L)
      )
      if (length(output)) attr(res, "bettermc_output") <- output
    }


    # make consecutive invocations of this wrapper fail early
    if (mc.fail.early && inherits(res, "etry-error")) file.create(error_file)

    if (length(warnings)) attr(res, "bettermc_warnings") <- warnings
    if (length(messages)) attr(res, "bettermc_messages") <- messages
    if (length(conditions)) attr(res, "bettermc_conditions") <- conditions

    if (!is.infinite(mc.compress.chars)) {
      res <- compress_chars(res, limit = mc.compress.chars,
                            compress_altreps = mc.compress.altreps)
    }

    if (!is.infinite(mc.share.vectors)) {
      res <- vectors2shm(res, limit = mc.share.vectors,
                         share_altreps = mc.share.altreps,
                         copy = mc.share.copy,
                         name_prefix = shm_prefix)
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

  # parallel-apply wrapper to seq_along(X)
  withCallingHandlers(
    res <- parallel::mclapply(
      X = seq_along(X), FUN = wrapper, ... = ...,
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

  # if there is an error in our wrapper code it will be caught by the
  # try-wrapper of parallel::mclapply; in this case we must always fail
  if (any(wrapper_error <-
          vapply(res, inherits, logical(1L), what = "try-error") &
          !vapply(res, inherits, logical(1L), what = "etry-error"))) {
    orig_message <- res[[which(wrapper_error)[1]]]
    msg <- "error in bettermc wrapper code; first original message:\n\n" %+%
      paste0(capture.output(orig_message), collapse = "\n")
    stop(msg)
  }

  # number of results affected by fatal error(s)
  # - in old versions of the parallel package, fatal error would make res to be
  #   shorter than X (cf. https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17343)
  # - in new versions the respective element(s) will be NULL
  mc_fatal <- length(X) - length(res) + sum(vapply(res, is.null, logical(1L)))

  if (mc.progress) {
    # ensure that the child process printing the progress bar is unblocked in
    # case of fatal errors (calling sem_post more often than actually required
    # does not harm)
    for (i in seq_len(mc_fatal)) {
      sem_post(sem)
    }
    parallel::mccollect(progress_job)
  }

  if (mc_fatal) {
    msg <- "at least one scheduled core did not return results;" %\%
      "maybe it was killed (be the Linux Out of Memory Killer ?) or there" %\%
      "was a fatal error in the forked process(es)"
    if (mc.allow.fatal) {
      warning(msg)
    } else {
      stop(msg)
    }
  }

  if (mc.shm.ipc) {
    res <- lapply(res, function(e) {
      if (inherits(e, "shm_obj")) {
        unserialize(allocate_from_shm(e))
      } else {
        # there was a fatal error and mc.allow.fatal == TRUE, i.e. e is NULL
        # - or -
        # we signalled an error (outside of etry), e.g. warning to error or
        # due to failing early
        # - or -
        # copy2shm failed in the child process
        e
      }
    })
  }

  if (!is.infinite(mc.share.vectors)) {
    res <- shm2vectors(res)
  }

  if (!is.infinite(mc.compress.chars)) {
    res <- uncompress_chars(res)
  }

  if (mc.stdout == "capture") {
    lapply(seq_along(res), function(i) {
      e <- res[[i]]
      if (!is.null(attr(e, "bettermc_output"))) {
        cat(paste0(sprintf("%5d: ", i) ,attr(e, "bettermc_output")), sep = "\n")
        attr(e, "bettermc_output") <- NULL
      }
    })
  }

  if (mc.warnings == "signal") {
    lapply(seq_along(res), function(i) {
      e <- res[[i]]
      if (!is.null(attr(e, "bettermc_warnings"))) {
        lapply(attr(e, "bettermc_warnings"), function(w) {
          w$message <- sprintf("%d: %s", i, w$message)
          warning(w)
        })
        attr(e, "bettermc_warnings") <- NULL
      }
    })
  }

  if (mc.messages == "signal") {
    lapply(seq_along(res), function(i) {
      e <- res[[i]]
      if (!is.null(attr(e, "bettermc_messages"))) {
        lapply(attr(e, "bettermc_messages"), function(m) {
          m$message <- sprintf("%5d: %s", i, m$message)
          message(m)
        })
        attr(e, "bettermc_messages") <- NULL
      }
    })
  }

  if (mc.conditions == "signal") {
    lapply(seq_along(res), function(i) {
      e <- res[[i]]
      if (!is.null(attr(e, "bettermc_conditions"))) {
        lapply(attr(e, "bettermc_conditions"), function(cond) {
          cond$message <- sprintf("%5d: %s", i, cond$message)
          signalCondition(cond)
        })
        attr(e, "bettermc_conditions") <- NULL
      }
    })
  }

  if (any(mc_error <- vapply(res, inherits, logical(1L), what = "etry-error"))) {
    orig_message <- res[[which(mc_error)[1]]]
    msg <- "error(s) occured during mclapply; first original message:\n\n" %+%
      paste0(capture.output(orig_message), collapse = "\n")
    if (mc.allow.error) {
      warning(msg)
    } else {
      if (mc.dump.frames != "no") {
        if (grepl("^file://", mc.dumpto)) {
          saveRDS(res, gsub("^file://", "", mc.dumpto))
        } else {
          assign(mc.dumpto, res, .GlobalEnv)
        }
      }
      stop(msg)
    }
  }

  # remove the list()-wrapper around each (non-error) element
  res <- lapply(res, function(e) if (inherits(e, "etry-error") || inherits(e, "error")) e else e[[1L]])

  # length check due to https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17343
  if (length(res) == length(X)) names(res) <- names(X)

  res
}

