test_that("tsfae06", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae06.R"), "tsfae06.rtf")
})
