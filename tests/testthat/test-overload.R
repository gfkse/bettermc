test_that("overloading works for doMC", {
  skip_on_os("windows")
  overload_mclapply(imports = "doMC", fixed_args = list(mc.set.seed = NA))

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
