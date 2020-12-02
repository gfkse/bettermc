for (copy in c(TRUE, FALSE)) {
  test_that("copy2shm & allocate_from_shm produce correct results", {
    expect_identical(numeric(), allocate_from_shm(copy2shm(numeric(), "/bettermc_copy2shm_test", copy = copy)))
    expect_identical(integer(), allocate_from_shm(copy2shm(integer(), "/bettermc_copy2shm_test", copy = copy)))
    expect_identical(raw(), allocate_from_shm(copy2shm(raw(), "/bettermc_copy2shm_test", copy = copy)))
    expect_identical(logical(), allocate_from_shm(copy2shm(logical(), "/bettermc_copy2shm_test", copy = copy)))
    expect_identical(complex(), allocate_from_shm(copy2shm(complex(), "/bettermc_copy2shm_test", copy = copy)))

    expect_identical(1, allocate_from_shm(copy2shm(1, "/bettermc_copy2shm_test", copy = copy)))
    expect_identical(1L, allocate_from_shm(copy2shm(1L, "/bettermc_copy2shm_test", copy = copy)))
    expect_identical(as.raw(1), allocate_from_shm(copy2shm(as.raw(1), "/bettermc_copy2shm_test", copy = copy)))
    expect_identical(TRUE, allocate_from_shm(copy2shm(TRUE, "/bettermc_copy2shm_test", copy = copy)))
    expect_identical(complex(1), allocate_from_shm(copy2shm(complex(1), "/bettermc_copy2shm_test", copy = copy)))

    expect_identical(c(1, 3, 5), allocate_from_shm(copy2shm(c(1, 3, 5), "/bettermc_copy2shm_test", copy = copy)))
    expect_identical(c(1L, 3L, 5L), allocate_from_shm(copy2shm(c(1L, 3L, 5L), "/bettermc_copy2shm_test", copy = copy)))
    expect_identical(as.raw(c(1, 3, 5)), allocate_from_shm(copy2shm(as.raw(c(1, 3, 5)), "/bettermc_copy2shm_test", copy = copy)))
    expect_identical(c(TRUE, FALSE, TRUE), allocate_from_shm(copy2shm(c(TRUE, FALSE, TRUE), "/bettermc_copy2shm_test", copy = copy)))
    expect_identical(complex(3), allocate_from_shm(copy2shm(complex(3), "/bettermc_copy2shm_test", copy = copy)))

    expect_identical(as.numeric(1:10), allocate_from_shm(copy2shm(as.numeric(1:10), "/bettermc_copy2shm_test", copy = copy)))
    expect_identical(1:10, allocate_from_shm(copy2shm(1:10, "/bettermc_copy2shm_test", copy = copy)))
    expect_identical(rep(TRUE, 10), allocate_from_shm(copy2shm(rep(TRUE, 10), "/bettermc_copy2shm_test", copy = copy)))
  })

  test_that("allocate_from_shm handles a corrupt shm_obj gracefully", {
    o <- copy2shm(numeric(), "/bettermc_allocate_from_shm_test", overwrite = TRUE, copy = copy)
    o$name <- "bettermc_allocate_from_shm_test_does_not_exist"
    expect_error(allocate_from_shm(o), "shm_open")

    o <- copy2shm(1:10, "/bettermc_allocate_from_shm_test", overwrite = TRUE, copy = copy)
    o$length <- 11
    expect_error(allocate_from_shm(o), "alloc_from_shm")

    o <- copy2shm(1:10, "/bettermc_allocate_from_shm_test", overwrite = TRUE, copy = copy)
    o$size <- 100
    expect_error(allocate_from_shm(o), "wrong size")

    o <- copy2shm(1:10, "/bettermc_allocate_from_shm_test", overwrite = TRUE, copy = copy)
    o$type <- 0
    expect_error(allocate_from_shm(o), "unsupported SEXP type")
  })
}

