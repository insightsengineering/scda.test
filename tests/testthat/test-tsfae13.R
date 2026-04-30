test_that("tsfae13", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae13.R"), "tsfae13.rtf")
})
