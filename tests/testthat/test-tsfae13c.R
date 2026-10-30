test_that("tsfae13c", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae13c.R"), "tsfae13c.rtf")
})
