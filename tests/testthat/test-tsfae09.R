test_that("tsfae09", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae09.R"), "tsfae09.rtf")
})
