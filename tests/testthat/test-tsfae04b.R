test_that("tsfae04b", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae04b.R"), "tsfae04b.rtf")
})
