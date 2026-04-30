test_that("tsfae17a", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae17a.R"), "tsfae17a.rtf")
})
