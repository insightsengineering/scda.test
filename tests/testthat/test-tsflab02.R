test_that("tsflab02", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsflab02.R"), "tsflab02.rtf")
})
