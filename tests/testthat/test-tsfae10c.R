test_that("tsfae10c", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae10c.R"), "tsfae10c.rtf")
})
