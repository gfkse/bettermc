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
