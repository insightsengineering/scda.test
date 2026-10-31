test_that("tsfae03c", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae03c.R"), "tsfae03c.rtf")
})
