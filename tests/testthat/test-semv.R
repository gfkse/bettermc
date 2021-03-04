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
