test_that("tsfae03d", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae03d.R"), "tsfae03d.rtf")
})
