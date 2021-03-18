test_that("mc.retry works", {
  set.seed(123)
  res <- suppressMessages(
    bettermc::mclapply(1:20, function(i) {
      r <- runif(1)
      if (r < 0.25)
        system(paste0("kill ", Sys.getpid()))
      else if (r < 0.5)
        stop(i)
      else
        i
    }, mc.retry = -50, mc.preschedule = TRUE, mc.cores = 2, mc.force.fork = TRUE)
  )
  expect_identical(res, as.list(1:20))

  res <- suppressMessages(
    bettermc::mclapply(1:20, function(i) {
      r <- runif(1)
      if (r < 0.25)
        system(paste0("kill ", Sys.getpid()))
      else if (r < 0.5)
        stop(i)
      else
        i
    }, mc.retry = 50, mc.preschedule = FALSE, mc.cores = 2, mc.force.fork = TRUE)
  )
  expect_identical(res, as.list(1:20))

  res <- suppressMessages(
    bettermc::mclapply(1:20, function(i) {
      r <- runif(1)
      if (r < 0.5)
        stop(i)
      else
        i
    }, mc.retry = 50, mc.preschedule = FALSE, mc.cores = 2, mc.force.fork = FALSE)
  )
  expect_identical(res, as.list(1:20))

  X <- as.list(1:20)
  names(X) <- LETTERS[seq_along(X)]
  res <- suppressMessages(
    bettermc::mclapply(X, function(i) {
      r <- runif(1)
      if (r < 0.5)
        stop(i)
      else
        i
    }, mc.retry = 50, mc.preschedule = FALSE, mc.cores = 2, mc.force.fork = TRUE)
  )
  expect_identical(res, X)

  res1 <- suppressMessages(
    bettermc::mclapply(1:20, function(i) {
      r <- runif(1)
      if (r < 0.25)
        system(paste0("kill ", Sys.getpid()))
      else if (r < 0.5)
        stop(i)
      else
        runif(1)
    }, mc.retry = 50, mc.preschedule = FALSE, mc.cores = 2, mc.force.fork = TRUE, mc.set.seed = 123)
  )

  res2 <- suppressMessages(
    bettermc::mclapply(1:20, function(i) {
      r <- runif(1)
      if (r < 0.25)
        system(paste0("kill ", Sys.getpid()))
      else if (r < 0.5)
        stop(i)
      else
        runif(1)
    }, mc.retry = 50, mc.preschedule = FALSE, mc.cores = 2, mc.force.fork = TRUE, mc.set.seed = 123)
  )

  set.seed(123)
  res3 <- suppressMessages(
    bettermc::mclapply(1:20, function(i) {
      r <- runif(1)
      if (r < 0.25)
        system(paste0("kill ", Sys.getpid()))
      else if (r < 0.5)
        stop(i)
      else
        runif(1)
    }, mc.retry = 50, mc.preschedule = FALSE, mc.cores = 2, mc.force.fork = TRUE, mc.set.seed = NA)
  )

  expect_identical(res1, res2)
  expect_identical(res1, res3)
})
