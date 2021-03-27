for (copy in c(TRUE, FALSE)) {
  test_that("copy2shm & allocate_from_shm produce correct results", {
    skip_on_os("windows")
    shm_name <- gen_posix_name()
    expect_identical(numeric(), allocate_from_shm(copy2shm(numeric(), shm_name, copy = copy)))
    expect_identical(integer(), allocate_from_shm(copy2shm(integer(), shm_name, copy = copy)))
    expect_identical(raw(), allocate_from_shm(copy2shm(raw(), shm_name, copy = copy)))
    expect_identical(logical(), allocate_from_shm(copy2shm(logical(), shm_name, copy = copy)))
    expect_identical(complex(), allocate_from_shm(copy2shm(complex(), shm_name, copy = copy)))

    expect_identical(1, allocate_from_shm(copy2shm(1, shm_name, copy = copy)))
    expect_identical(1L, allocate_from_shm(copy2shm(1L, shm_name, copy = copy)))
    expect_identical(as.raw(1), allocate_from_shm(copy2shm(as.raw(1), shm_name, copy = copy)))
    expect_identical(TRUE, allocate_from_shm(copy2shm(TRUE, shm_name, copy = copy)))
    expect_identical(complex(1), allocate_from_shm(copy2shm(complex(1), shm_name, copy = copy)))

    expect_identical(c(1, 3, 5), allocate_from_shm(copy2shm(c(1, 3, 5), shm_name, copy = copy)))
    expect_identical(c(1L, 3L, 5L), allocate_from_shm(copy2shm(c(1L, 3L, 5L), shm_name, copy = copy)))
    expect_identical(as.raw(c(1, 3, 5)), allocate_from_shm(copy2shm(as.raw(c(1, 3, 5)), shm_name, copy = copy)))
    expect_identical(c(TRUE, FALSE, TRUE), allocate_from_shm(copy2shm(c(TRUE, FALSE, TRUE), shm_name, copy = copy)))
    expect_identical(complex(3), allocate_from_shm(copy2shm(complex(3), shm_name, copy = copy)))

    expect_identical(as.numeric(1:10), allocate_from_shm(copy2shm(as.numeric(1:10), shm_name, copy = copy)))
    expect_identical(1:10, allocate_from_shm(copy2shm(1:10, shm_name, copy = copy)))
    expect_identical(rep(TRUE, 10), allocate_from_shm(copy2shm(rep(TRUE, 10), shm_name, copy = copy)))
  })

  test_that("allocate_from_shm handles a corrupt shm_obj gracefully", {
    skip_on_os("windows")
    shm_name <- gen_posix_name()

    o <- copy2shm(numeric(), shm_name, copy = copy)
    o$name <- "/bmc_missing_test"
    expect_error(allocate_from_shm(o), "shm_open")
    o$name <- shm_name
    allocate_from_shm(o)

    o <- copy2shm(1:10, shm_name, copy = copy)
    o$length <- 11
    expect_error(allocate_from_shm(o), "alloc_from_shm")

    o <- copy2shm(1:10, shm_name, copy = copy)
    o$type <- 0
    expect_error(allocate_from_shm(o), "unsupported SEXP type")

    o <- copy2shm(1:10, shm_name, copy = copy)
    o$size <- 1024 * 1024 + 1  # must be larger than page size on macOS
    expect_error(allocate_from_shm(o), "wrong size")
  })

  test_that("allocate_from_shm checks exact size of shm obj on Linux", {
    skip_on_os("windows")
    skip_on_os("mac")

    o <- copy2shm(1:10, gen_posix_name(), copy = copy)
    o$size <- 100
    expect_error(allocate_from_shm(o), "wrong size")
  })

  test_that("changes to vectors allocate(d)_from_shm are private", {
    skip_on_os("windows")
    o <- copy2shm(1:10, name = gen_posix_name(), copy = copy)
    x <- allocate_from_shm(o)
    parallel::mclapply(1:2, function(i) x[i] <<- 99L)
    expect_identical(x, 1:10)
  })

  test_that("overwrite in allocate_from_shm works", {
    skip_on_os("windows")
    skip_on_os("mac")

    shm_name <- gen_posix_name()
    copy2shm(c(1, 2, 3), shm_name, copy = copy)
    o <- copy2shm(c(4, 5, 6), shm_name, copy = copy, overwrite = TRUE)
    expect_identical(allocate_from_shm(o), c(4, 5, 6))
  })
}

test_that("MADV_HUGEPAGE does not cause issues", {
  skip_on_os("windows")
  opt_bak <- options(bettermc.hugepage_limit = 0L)
  expect_identical(allocate_from_shm(copy2shm(1:10, gen_posix_name())), 1:10)
  options(opt_bak)
})
