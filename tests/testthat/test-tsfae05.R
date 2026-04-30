test_that("tsfae05", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae05.R"), "tsfae05.rtf")
})
