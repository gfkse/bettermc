mc.share.vectors <- c(TRUE, FALSE)
mc.compress.chars <- c(TRUE, FALSE)
mc.compress.altreps <- c("if_allocated", "yes", "no")
mc.share.altreps <- c("no", "yes", "if_allocated")
mc.share.copy <- c(TRUE, FALSE)
mc.shm.ipc <- c(TRUE, FALSE)
mc.progress <- c(FALSE, TRUE)
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
  test_that(paste0(idx, ": ", paste0(names(args), ": ", args[idx, ], collapse = " - ")), {
    X_test <- c(X, list(X), lapply(X, asS4),
                chmap = list(char_map(letters)),
                shm1 = list(copy2shm(1:10, paste0("/bettermc_test_copy1_", idx))),
                shm2 = list(copy2shm(1:10, paste0("/bettermc_test_direct1_", idx), copy = FALSE)))

    X_exp <- X_test

    if (args[idx, "mc.share.vectors"]) {
      X_exp$shm1 <- 1:10
      X_exp$shm2 <- 1:10
    }

    if (args[idx, "mc.compress.chars"]) {
      X_exp$chmap <- letters
    }

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

    if (!args[idx, "mc.share.vectors"]) {
      bettermc:::unlink_all_shm("/bettermc_test_copy1_", idx)
      bettermc:::unlink_all_shm("/bettermc_test_direct1_", idx)
    }


    X_test <- c(X, list(X), lapply(X, asS4),
                chmap = list(char_map(letters)),
                shm1 = list(copy2shm(1:10, paste0("/bettermc_test_copy2_", idx))),
                shm2 = list(copy2shm(1:10, paste0("/bettermc_test_direct2_", idx), copy = FALSE)))

    X_exp <- X_test

    if (args[idx, "mc.share.vectors"]) {
      X_exp$shm1 <- 1:10
      X_exp$shm2 <- 1:10
    }

    if (args[idx, "mc.compress.chars"]) {
      X_exp$chmap <- letters
    }

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

    if (!args[idx, "mc.share.vectors"]) {
      bettermc:::unlink_all_shm("/bettermc_test_copy2_", idx)
      bettermc:::unlink_all_shm("/bettermc_test_direct2_", idx)
    }

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
  expect_error(bettermc::mclapply(1:2, function(i) system(sprintf("kill %d", Sys.getpid())), mc.shm.ipc = TRUE),
               "at least one scheduled core did not return results", fixed = TRUE)
  expect_error(bettermc::mclapply(1:2, function(i) system(sprintf("kill %d", Sys.getpid())), mc.shm.ipc = FALSE),
               "at least one scheduled core did not return results", fixed = TRUE)

  ret <- expect_warning(bettermc::mclapply(1:2, function(i) system(sprintf("kill %d", Sys.getpid())), mc.allow.fatal = TRUE, mc.shm.ipc = TRUE),
                        "at least one scheduled core did not return results", fixed = TRUE)
  expect_identical(ret, list(NULL, NULL))

  ret <- expect_warning(bettermc::mclapply(1:2, function(i) system(sprintf("kill %d", Sys.getpid())), mc.allow.fatal = TRUE, mc.shm.ipc = FALSE),
                        "at least one scheduled core did not return results", fixed = TRUE)
  expect_identical(ret, list(NULL, NULL))
})

test_that("mclapply handles warnings correctly", {
  expect_warning(bettermc::mclapply(1:2, function(i) warning(i)),
               "1: 1", fixed = TRUE)
  expect_error(bettermc::mclapply(1:2, function(i) warning(i), mc.warnings = "stop"),
               "(converted from warning)", fixed = TRUE)
  expect_silent(bettermc::mclapply(1:2, function(i) warning(i), mc.warnings = "ignore"))
})