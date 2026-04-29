test_that("tsfae17c", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae17c.R"), "tsfae17c.rtf")
})
