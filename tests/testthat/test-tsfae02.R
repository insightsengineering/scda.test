test_that("tsfae02", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae02.R"), "tsfae02.rtf")
})
