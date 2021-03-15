test_that("system v semaphores work", {
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
  sid <- semv_open(1)
  job <- parallel::mcparallel({semv_wait(sid); semv_wait(sid); TRUE})
  expect_null(parallel::mccollect(job, wait = FALSE, timeout = 1))  # i.e. blocking
  parallel:::mckill(job, signal = tools::SIGKILL)
  semv_wait(sid)  # will only return if first wait in child was undone after kill
  semv_unlink(sid)
})