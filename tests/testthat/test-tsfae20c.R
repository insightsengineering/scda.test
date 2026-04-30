test_that("tsfae20c", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae20c.R"), "tsfae20c.rtf")
})
