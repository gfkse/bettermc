test_that("system v semaphores work", {
  skip_on_os("windows")
  sid <- semv_open(1)
  semv_wait(sid)
  semv_post(sid)
  semv_wait(sid)
  semv_unlink(sid)

  expect_error(semv_wait(sid))
  expect_error(semv_post(sid))
  expect_error(semv_unlink(sid))
  expect_error(semv_open(-1))
})

test_that("undo works for system v semaphores", {
  skip_on_os("windows")
  sid <- semv_open(1)
  job <- parallel::mcparallel({semv_wait(sid); semv_wait(sid); TRUE})
  expect_null(parallel::mccollect(job, wait = FALSE, timeout = 1))  # i.e. blocking
  parallel:::mckill(job, signal = tools::SIGKILL)
  semv_wait(sid)  # will only return if first wait in child was undone after kill
  semv_unlink(sid)
})

test_that("system v semaphores are interruptible", {
  skip_on_os("windows")
  sid <- semv_open(0)
  ppid <- Sys.getpid()
  job <- parallel::mcparallel({Sys.sleep(1); system(paste0("kill -", tools::SIGINT, " ", ppid))})
  expect_true(tryCatch(semv_wait(sid), interrupt = function(i) TRUE))
  expect_identical(parallel::mccollect(job)[[1]], 0L)
  semv_unlink(sid)
})
