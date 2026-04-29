test_that("tsfae21c", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae21c.R"), "tsfae21c.rtf")
})
