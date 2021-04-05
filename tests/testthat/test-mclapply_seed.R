test_that("seeding works", {
  set.seed(456)
  ret1 <- bettermc::mclapply(1:4, function(i) runif(i))
  ret2 <- bettermc::mclapply(1:4, function(i) runif(i), mc.cores = 1, mc.set.seed = 456)
  ret3 <- bettermc::mclapply(1:4, function(i) runif(i), mc.cores = 1, mc.set.seed = 456, mc.retry = 5)
  set.seed(456)
  ret4 <- bettermc::mclapply(1:4, function(i) runif(i), mc.cores = 2, mc.set.seed = NA, mc.retry = -3)
  ret5 <- bettermc::mclapply(1:4, function(i) runif(i), mc.cores = 1, mc.set.seed = 456, mc.preschedule = FALSE)

  expect_known_output(print(ret1), test_path("known/seeding_res"))
  expect_identical(ret2, ret1)
  expect_identical(ret3, ret1)
  expect_identical(ret4, ret1)
  expect_identical(ret5, ret1)
})

test_that("RNG state outside of mclapply is not affected", {
  set.seed(987)
  x <- runif(10)

  set.seed(987)
  bettermc::mclapply(1:4, function(i) runif(i))
  y <- runif(10)

  set.seed(987)
  bettermc::mclapply(1:4, function(i) runif(i), mc.set.seed = 123)
  z <- runif(10)

  expect_identical(y, x)
  expect_identical(z, x)


  # test case where there is no .Random.seed
  rm(list = ".Random.seed", pos = .GlobalEnv)
  bettermc::mclapply(1:4, function(i) runif(i), mc.set.seed = 123)
})
