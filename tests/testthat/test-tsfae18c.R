test_that("tsfae18c", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae18c.R"), "tsfae18c.rtf")
})
