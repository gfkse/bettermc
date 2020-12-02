test_that("char_map works in edge cases", {
  x <- character()
  ret <- char_map(x)
  expect_identical(x, ret$chars[ret$idx])

  x <- NA_character_
  ret <- char_map(x)
  expect_identical(x, ret$chars[ret$idx])

  x <- c(NA_character_, as.character(runif(1e4)), NA_character_)
  ret <- char_map(x)
  expect_identical(x, ret$chars[ret$idx])
})

test_that("char_map produces correct results", {
  x <- sample(c(as.character(runif(1e3)), NA_character_), 1e6, replace = TRUE)
  ret <- char_map(x)
  expect_identical(x, ret$chars[ret$idx])
})


char_map_long <- function(x) {
  structure(.Call(C_char_map_long, x), names = c("chars", "idx", "attributes"),
            class = "char_map")
}
test_that("char_map_long works in edge cases", {
  x <- character()
  ret <- char_map_long(x)
  expect_identical(x, ret$chars[ret$idx])

  x <- NA_character_
  ret <- char_map_long(x)
  expect_identical(x, ret$chars[ret$idx])

  x <- c(NA_character_, as.character(runif(1e4)), NA_character_)
  ret <- char_map_long(x)
  expect_identical(x, ret$chars[ret$idx])
})

test_that("char_map_long produces correct results", {
  x <- sample(c(as.character(runif(1e3)), NA_character_), 1e6, replace = TRUE)
  ret <- char_map_long(x)
  expect_identical(x, ret$chars[ret$idx])
})
