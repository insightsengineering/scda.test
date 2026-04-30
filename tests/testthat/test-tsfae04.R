test_that("tsfae04", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae04.R"), "tsfae04.rtf")
})
