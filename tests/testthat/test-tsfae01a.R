test_that("tsfae01a", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae01a.R"), "tsfae01a.rtf")
})
