test_that("tsfae11c", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae11c.R"), "tsfae11c.rtf")
})
