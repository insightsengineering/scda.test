test_that("tsfae04a", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae04a.R"), "tsfae04a.rtf")
})
