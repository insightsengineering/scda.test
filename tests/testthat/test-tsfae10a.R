test_that("tsfae10a", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae10a.R"), "tsfae10a.rtf")
})
