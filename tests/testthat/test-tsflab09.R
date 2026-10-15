test_that("tsflab09", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsflab09.R"), "tsflab09.rtf")
})
