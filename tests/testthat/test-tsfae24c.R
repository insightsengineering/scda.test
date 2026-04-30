test_that("tsfae24c", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae24c.R"), "tsfae24c.rtf")
})
