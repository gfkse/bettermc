mc.share.vectors <- c(TRUE, FALSE)
mc.compress.chars <- c(TRUE, FALSE)
mc.compress.altreps <- c("if_allocated", "yes", "no")
mc.share.altreps <- c("no", "yes", "if_allocated")
mc.share.copy <- c(TRUE, FALSE)
mc.shm.ipc <- c(TRUE, FALSE)
mc.progress <- c(FALSE)
mc.cores <- c(1, 2)
mc.stdout <- c("capture", "output")
mc.silent <- c(TRUE, FALSE)
args <- expand.grid(mc.share.vectors = mc.share.vectors,
                    mc.compress.chars = mc.compress.chars,
                    mc.compress.altreps = mc.compress.altreps,
                    mc.share.altreps = mc.share.altreps,
                    mc.share.copy = mc.share.copy,
                    mc.shm.ipc = mc.shm.ipc,
                    mc.progress = mc.progress,
                    mc.cores = mc.cores,
                    mc.stdout = mc.stdout,
                    mc.silent = mc.silent,
                    warning = c(TRUE, FALSE),
                    stringsAsFactors = FALSE)

X <- list(1L, 2, as.raw(3), 1:10, letters, as.numeric(1:10), env = .GlobalEnv, c(1L, 3L, 5L),
          as.character(1:10), c(5, 7, 9), as.raw(1:10), complex(5, 1:5, 5:1), NULL, try(stop("try"), silent = TRUE),
          structure("etry", class = "etry-error"))

start_time <- Sys.time()
for (idx in sample.int(nrow(args))) {
  if (Sys.time() - start_time > 20) break

  shm_prefix_copy1 <- gen_posix_name("bmc_c1")
  shm_prefix_direct1 <- gen_posix_name("bmc_d1")

  test_that(paste0(idx, ": ", paste0(names(args), ": ", args[idx, ], collapse = " - ")), {
    X_test <- c(X, list(X), lapply(X, asS4),
                chmap = list(char_map(letters)),
                shm1 = if (bettermc:::OSTYPE != "windows") list(copy2shm(1:10, paste0(shm_prefix_copy1, idx))),
                shm2 = if (bettermc:::OSTYPE != "windows") list(copy2shm(1:10, paste0(shm_prefix_direct1, idx), copy = FALSE)))

    X_exp <- X_test

    if (args[idx, "warning"]) {
      expect_warning(
        expect_identical(do.call(bettermc::mclapply, c(X = list(seq_along(X_test)),
                                                       FUN = function(i) {
                                                         if (args[idx, "warning"]) warning("WWW")
                                                         X_test[[i]]
                                                       },
                                                       as.list(args[idx, grep("^mc\\.", names(args))]))),
                         parallel::mclapply(seq_along(X_exp), function(i) X_exp[[i]])),
        regexp = "WWW"
      )
    } else {
      expect_silent(
        expect_identical(do.call(bettermc::mclapply, c(X = list(seq_along(X_test)),
                                                       FUN = function(i) {
                                                         if (args[idx, "warning"]) warning("WWW")
                                                         X_test[[i]]
                                                       },
                                                       as.list(args[idx, grep("^mc\\.", names(args))]))),
                         parallel::mclapply(seq_along(X_exp), function(i) X_exp[[i]]))
      )
    }

    if (bettermc:::OSTYPE != "windows") bettermc:::unlink_all_shm(shm_prefix_copy1, idx)
    if (bettermc:::OSTYPE != "windows") bettermc:::unlink_all_shm(shm_prefix_direct1, idx)

    shm_prefix_copy2 <- gen_posix_name("bmc_c2")
    shm_prefix_direct2 <- gen_posix_name("bmc_d2")

    X_test <- c(X, list(X), lapply(X, asS4),
                chmap = list(char_map(letters)),
                shm1 = if (bettermc:::OSTYPE != "windows") list(copy2shm(1:10, paste0(shm_prefix_copy2, idx))),
                shm2 = if (bettermc:::OSTYPE != "windows") list(copy2shm(1:10, paste0(shm_prefix_direct2, idx), copy = FALSE)))

    X_exp <- X_test

    if (args[idx, "warning"]) {
      expect_warning(
        expect_identical(do.call(bettermc::mclapply, c(X = list(X_test),
                                                       FUN = function(x) {
                                                         if (args[idx, "warning"]) warning("WWW")
                                                         x
                                                       },
                                                       as.list(args[idx, grep("^mc\\.", names(args))]))),
                         parallel::mclapply(X_exp, identity)),
        regexp = "WWW"
      )
    } else {
      expect_silent(
        expect_identical(do.call(bettermc::mclapply, c(X = list(X_test),
                                                       FUN = function(x) {
                                                         if (args[idx, "warning"]) warning("WWW")
                                                         x
                                                       },
                                                       as.list(args[idx, grep("^mc\\.", names(args))]))),
                         parallel::mclapply(X_exp, identity))
      )
    }

    if (bettermc:::OSTYPE != "windows") bettermc:::unlink_all_shm(shm_prefix_copy2, idx)
    if (bettermc:::OSTYPE != "windows") bettermc:::unlink_all_shm(shm_prefix_direct2, idx)

  })
}

names(X) <- letters[seq_along(X)]
E <- list2env(X)
E[["E"]] <- E
test_that("returning recursive environments using shared memory works", {
  expect_equal(
    bettermc::mclapply(1:2, function(i) E, mc.share.vectors = TRUE, mc.share.altreps = "yes"),
    parallel::mclapply(1:2, function(i) E)
  )
})

test_that("mc.progress works", {
  if (requireNamespace("progress", quietly = TRUE)) {
    expect_silent(
      bettermc::mclapply(1:2, function(i) i, mc.progress = TRUE, mc.cores = 1)
    )
  } else {
    expect_message(
      bettermc::mclapply(1:2, function(i) i, mc.progress = TRUE, mc.cores = 1),
      "Please install the progress-package in order to get a progress bar"
    )
  }
})

test_that("returning special vaules works correctly", {
  env <- new.env()
  env[["locked"]] <- c(1, 3, 5)
  lockBinding("locked", env)
  makeActiveBinding("active", function(x) "active", env)
  env[["missing"]] <- quote(expr = )
  lockEnvironment(env)

  expect_equal(bettermc::mclapply(1:2, function(i) env),
               list(env, env))
  expect_equal(bettermc::mclapply(1:2, function(i) as.list(env)),
               list(as.list(env), as.list(env)))

  env <- new.env()
  delayedAssign("promise", stop("I am a promise!"), env, env)
  expect_silent(bettermc::mclapply(1:2, function(i) env))
})

test_that("mclapply handles non-fatal error correctly", {
  expect_error(bettermc::mclapply(1:2, function(i) stop(i), mc.shm.ipc = TRUE),
               "error(s) occured during mclapply; first original message", fixed = TRUE)
  expect_error(bettermc::mclapply(1:2, function(i) stop(i), mc.shm.ipc = FALSE),
               "error(s) occured during mclapply; first original message", fixed = TRUE)
  ret <- expect_warning(bettermc::mclapply(1:2, function(i) stop(i), mc.allow.error = TRUE),
                        "error(s) occured during mclapply; first original message", fixed = TRUE)
  expect_length(ret, 2)
  expect_s3_class(ret[[1]], "etry-error")
  expect_s3_class(ret[[1]], "try-error")
})

test_that("mclapply handles fatal error correctly", {
  skip_on_os("windows")
  expect_error(bettermc::mclapply(1:2, function(i) system(sprintf("kill %d", Sys.getpid())), mc.shm.ipc = TRUE),
               "at least one scheduled core did not return results", fixed = TRUE)
  expect_error(bettermc::mclapply(1:2, function(i) system(sprintf("kill %d", Sys.getpid())), mc.shm.ipc = FALSE),
               "at least one scheduled core did not return results", fixed = TRUE)

  ret <- expect_warning(bettermc::mclapply(1:2, function(i) system(sprintf("kill %d", Sys.getpid())), mc.allow.fatal = NULL, mc.shm.ipc = TRUE),
                        "at least one scheduled core did not return results", fixed = TRUE)
  expect_identical(ret, list(NULL, NULL))

  ret <- expect_warning(bettermc::mclapply(1:2, function(i) system(sprintf("kill %d", Sys.getpid())), mc.allow.fatal = NULL, mc.shm.ipc = FALSE),
                        "at least one scheduled core did not return results", fixed = TRUE)
  expect_identical(ret, list(NULL, NULL))

  ret <- expect_warning(bettermc::mclapply(1:2, function(i) system(sprintf("kill %d", Sys.getpid())), mc.allow.fatal = TRUE, mc.shm.ipc = TRUE),
                        "at least one scheduled core did not return results", fixed = TRUE)
  expect_true(all(sapply(ret, inherits, what = "fatal-error")))

  ret <- expect_warning(bettermc::mclapply(1:2, function(i) system(sprintf("kill %d", Sys.getpid())), mc.allow.fatal = TRUE, mc.shm.ipc = FALSE),
                        "at least one scheduled core did not return results", fixed = TRUE)
  expect_true(all(sapply(ret, inherits, what = "fatal-error")))
})

test_that("mc.fail.early works", {
  ret <- suppressWarnings(bettermc::mclapply(1:10, function(i) stop(i), mc.allow.error = TRUE, mc.fail.early = TRUE))
  expect_true(sum(sapply(ret, inherits, what = "fail-early-error")) >= 8)
})

test_that("joint fatal and non-fatal errors are handled correctly", {
  skip_on_os("windows")
  expect_error(bettermc::mclapply(1:2, function(i) {
    if (i == 1) {
      system(paste0("kill ", Sys.getpid()))
    } else {
      stop(i)
    }
  }, mc.fail.early = FALSE), regexp = "--- AND ---")

  expect_error(
    expect_warning(
      bettermc::mclapply(1:2, function(i) {
        if (i == 1) {
          system(paste0("kill ", Sys.getpid()))
        } else {
          stop(i)
        }
      }, mc.allow.error = TRUE, mc.fail.early = FALSE),
      regexp = "stop\\(i\\)"
    ),
    regexp = "Out of Memory Killer"
  )

  expect_error(
    expect_warning(
      bettermc::mclapply(1:2, function(i) {
        if (i == 1) {
          system(paste0("kill ", Sys.getpid()))
        } else {
          stop(i)
        }
      }, mc.allow.fatal = TRUE, mc.fail.early = FALSE),
      regexp = "Out of Memory Killer"
    ),
    regexp = "stop\\(i\\)"
  )

  expect_warning(
    bettermc::mclapply(1:2, function(i) {
      if (i == 1) {
        system(paste0("kill ", Sys.getpid()))
      } else {
        stop(i)
      }
    }, mc.allow.fatal = NULL, mc.allow.error = TRUE, mc.fail.early = FALSE),
    regexp = "stop\\(i\\)|Out of Memory Killer"
  )
})

test_that("mclapply handles warnings correctly", {
  skip_on_os("windows")
  expect_warning(bettermc::mclapply(1:2, function(i) warning(i)),
               "1: 1", fixed = TRUE)
  expect_error(bettermc::mclapply(1:2, function(i) warning(i), mc.warnings = "stop", mc.fail.early = FALSE),
               "(converted from warning)", fixed = TRUE)
  expect_silent(bettermc::mclapply(1:2, function(i) warning(i), mc.warnings = "ignore"))

  ppid <- Sys.getpid()
  expect_warning(
    withCallingHandlers({
      bettermc::mclapply(1:2, function(i) warning(i), mc.warnings = "m_signal")
      bettermc::mclapply(1:2, function(i) warning(i), mc.warnings = "m_output")
      bettermc::mclapply(1:2, function(i) warning(i), mc.warnings = "m_ignore")
    }, warning = function(w) if (Sys.getpid() != ppid) stop())
  )

  expect_error(
    tryCatch({
      bettermc::mclapply(1:2, function(i) warning(i), mc.warnings = "signal")
    }, warning = function(w) NULL)
  )

  expect_error(
    tryCatch({
      bettermc::mclapply(1:2, function(i) warning(i), mc.warnings = "output")
    }, warning = function(w) NULL)
  )

  expect_error(
    tryCatch({
      bettermc::mclapply(1:2, function(i) warning(i), mc.warnings = "ignore")
    }, warning = function(w) NULL)
  )
})

test_that("mclapply handles messages correctly", {
  skip_on_os("windows")
  expect_message(bettermc::mclapply(1:2, function(i) message(i)),
                 "1: 1", fixed = TRUE)
  expect_silent(bettermc::mclapply(1:2, function(i) message(i), mc.messages = "ignore"))

  ppid <- Sys.getpid()
  expect_message(
    withCallingHandlers({
      bettermc::mclapply(1:2, function(i) message(i), mc.messages = "m_signal")
      bettermc::mclapply(1:2, function(i) message(i), mc.messages = "m_output")
      bettermc::mclapply(1:2, function(i) message(i), mc.messages = "m_ignore")
    }, message = function(m) if (Sys.getpid() != ppid) stop())
  )

  expect_error(
    tryCatch({
      bettermc::mclapply(1:2, function(i) message(i), mc.messages = "signal")
    }, message = function(m) NULL)
  )

  expect_error(
    tryCatch({
      bettermc::mclapply(1:2, function(i) message(i), mc.messages = "output")
    }, message = function(m) NULL)
  )

  expect_error(
    tryCatch({
      bettermc::mclapply(1:2, function(i) message(i), mc.messages = "ignore")
    }, message = function(m) NULL)
  )
})

test_that("mclapply handles conditions correctly", {
  skip_on_os("windows")
  expect_silent(
    withCallingHandlers(bettermc::mclapply(1:2, function(i) signalCondition(simpleCondition(i)),
                                           mc.conditions = "ignore", mc.stdout = "output"),
                        condition = function(cond) print(cond))
  )

  expect_output(
    withCallingHandlers(bettermc::mclapply(1:2, function(i) signalCondition(simpleCondition(i)),
                                           mc.conditions = "signal", mc.stdout = "output"),
                        condition = function(cond) print(cond)),
    regexp = "simpleCondition"
  )

  expect_silent(
    withCallingHandlers(bettermc::mclapply(1:2, function(i) warning(i),
                                           mc.conditions = "signal", mc.warnings = "ignore"),
                        condition = function(cond) if (!inherits(cond, "warning")) print(cond))
  )
})

test_that("... is not forcefully evaluated", {
  expect_identical(bettermc::mclapply(1:2, function(i, j) i, j = stop("eee")),
                   list(1L, 2L))
})

test_that("mclapply works in edge cases", {
  expect_identical(bettermc::mclapply(NULL, I),
                   parallel::mclapply(NULL, I))

  expect_identical(bettermc::mclapply(numeric(), I, some_arg = 123),
                   parallel::mclapply(numeric(), I, some_arg = 123))

  expect_identical(bettermc::mclapply(list(), I),
                   parallel::mclapply(list(), I))

  nl <- list()
  names(nl) <- character()
  expect_identical(bettermc::mclapply(nl, I),
                   parallel::mclapply(nl, I))

  expect_identical(bettermc::mclapply(list(NULL), function(x) x),
                   parallel::mclapply(list(NULL), function(x) x))

  expect_identical(bettermc::mclapply(list(NULL, NULL), function(x) x),
                   parallel::mclapply(list(NULL, NULL), function(x) x))

  al <- structure(list(), some_attr = 951)
  expect_identical(bettermc::mclapply(al, function(x) x),
                   parallel::mclapply(al, function(x) x))

  al <- structure(list(character()), some_attr = 951)
  expect_identical(bettermc::mclapply(al, function(x) x),
                   parallel::mclapply(al, function(x) x))

  expect_identical(names(bettermc::mclapply(character(), function(x) x)),
                   character())
})

test_that("mc.force.fork correctly adjusts affinity.list", {
  expect_silent(bettermc::mclapply(1, function(i) i, mc.preschedule = FALSE, mc.force.fork = TRUE, affinity.list = NULL))
  expect_silent(bettermc::mclapply(1, function(i) i, mc.preschedule = FALSE, mc.force.fork = TRUE, affinity.list = 1))
  expect_silent(bettermc::mclapply(1, function(i) i, mc.preschedule = FALSE, mc.force.fork = TRUE, affinity.list = list(1)))
  expect_silent(bettermc::mclapply(1, function(i) i, mc.preschedule = FALSE, mc.force.fork = TRUE, affinity.list = list(c(1, 2))))
})

test_that("results are properly named", {
  expect_identical(names(bettermc::mclapply(letters, function(x) x)),
                   letters)

  X <- letters
  names(X) <- seq_along(X)
  expect_identical(names(bettermc::mclapply(X, function(x) x)),
                   names(X))
})

test_that("mc.system.time works", {
  skip_on_os("windows")
  ret <- suppressWarnings(
    bettermc::mclapply(1:4, function(i) {
      if (i == 3) {
        system(sprintf("kill %d", Sys.getpid()))
      } else if (i == 4) {
        stop("eee")
      } else {
        Sys.sleep(i)
      }
    },
    mc.system.time = TRUE, mc.allow.error = TRUE,
    mc.allow.fatal = TRUE, mc.preschedule = FALSE)
  )

  ret <- attr(ret, "system_times")
  ret <- lapply(ret, `[`, i = "elapsed")

  expect_gte(ret[[1]], 1)
  expect_gte(ret[[2]], 2)
  expect_null(ret[[3]])
  expect_lt(ret[[4]], 1)
})
