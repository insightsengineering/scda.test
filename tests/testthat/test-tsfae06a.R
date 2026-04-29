test_that("tsfae06a", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae06a.R"), "tsfae06a.rtf")
})
