test_that("overloading works for doMC", {
  skip_on_os("windows")
  overload_mclapply(imports = "doMC", fixed_args = list(mc.set.seed = NA),
                    defaults = list(mc.dump.frames = "no"))

  doMC::registerDoMC(2L)

  set.seed(123)
  ret1 <- foreach::`%dopar%`(foreach::foreach(i = 1:4), runif(1))
  set.seed(123)
  ret2 <- foreach::`%dopar%`(foreach::foreach(i = 1:4), runif(1))
  expect_identical(ret1, ret2)

  undo_overload_mclapply(imports = "doMC")

  set.seed(123)
  ret1 <- foreach::`%dopar%`(foreach::foreach(i = 1:4), runif(1))
  set.seed(123)
  ret2 <- foreach::`%dopar%`(foreach::foreach(i = 1:4), runif(1))
  expect_true(!identical(ret1, ret2))
})

test_that("overloading mclapply in namespace:parallel works", {
  overload_mclapply(parallel_namespace = TRUE)
  expect_identical(environment(parallel::mclapply), asNamespace("bettermc"))
  undo_overload_mclapply(parallel_namespace = TRUE)
  expect_identical(environment(parallel::mclapply), asNamespace("parallel"))
})

test_that("overloading mclapply in package:parallel works", {
  expect_silent(overload_mclapply(parallel_package = TRUE))
  library(parallel)
  overload_mclapply(parallel_package = TRUE)
  expect_identical(environment(as.environment("package:parallel")[["mclapply"]]),
                   asNamespace("bettermc"))
  undo_overload_mclapply(parallel_package = TRUE)
  expect_identical(environment(as.environment("package:parallel")[["mclapply"]]),
                   asNamespace("parallel"))
  detach("package:parallel")
})
