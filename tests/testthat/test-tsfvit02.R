test_that("tsfvit02", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfvit02.R"), "tsfvit02.rtf")
})
