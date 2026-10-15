test_that("tsfae18d", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae18d.R"), "tsfae18d.rtf")
})
