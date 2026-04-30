test_that("tsfae14", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae14.R"), "tsfae14.rtf")
})
