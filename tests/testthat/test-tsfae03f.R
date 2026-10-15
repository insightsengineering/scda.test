test_that("tsfae03f", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae03f.R"), "tsfae03f.rtf")
})
