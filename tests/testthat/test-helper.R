e <- new.env(parent = emptyenv())
assign(" ", NULL, e)
assign("5", 5, e)
assign("a\nb", "ab", e)
delayedAssign("delayed\nassign", 99, e, e)
assign("mis\tsing", quote(expr = ), e)

test_that("is.missing.arg works for exotic variable names", {
  expect_identical(lapply(ls(e), is.missing.arg, env = e),
                   list(FALSE, FALSE, FALSE, FALSE, TRUE))
})


test_that("is.uneval.promise works for exotic variable names", {
  expect_identical(lapply(ls(e), is.uneval.promise, env = e),
                   list(FALSE, FALSE, FALSE, TRUE, FALSE))
})
