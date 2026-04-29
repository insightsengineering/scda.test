test_that("tsfae10", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae10.R"), "tsfae10.rtf")
})
