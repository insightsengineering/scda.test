test_that("tsfae04c", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae04c.R"), "tsfae04c.rtf")
})
