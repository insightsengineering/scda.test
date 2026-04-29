test_that("tsfae08", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae08.R"), "tsfae08.rtf")
})
