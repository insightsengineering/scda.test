test_that("tsfae06b", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae06b.R"), "tsfae06b.rtf")
})
