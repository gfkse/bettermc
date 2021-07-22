test_that("(de)compression works for arbitrary objects", {
  x <- .GlobalEnv
  expect_identical(x, uncompress_chars(compress_chars(x)))

  x <- list(structure(list(), class = "myclass"))
  expect_identical(x, uncompress_chars(compress_chars(x)))

  x <- matrix(letters, 2)
  expect_identical(x, uncompress_chars(compress_chars(x)))

  x <- list(array(letters, c(2, 2, 13)), list(NULL,
                                              list(array(letters, c(2, 2, 13)),
                                                   LETTERS)))
  expect_identical(x, uncompress_chars(compress_chars(x)))
})

test_that("set_attr handles objects correctly", {
  d <- Sys.Date()
  attr(d, "test_atr") <- "test_val"
  dd <- d
  attributes(dd) <- NULL
  expect_false(identical(dd, d))

  .Call(C_set_attr, dd, as.pairlist(attributes(d)))
  expect_true(is.object(dd))
  expect_identical(dd, d)
})
