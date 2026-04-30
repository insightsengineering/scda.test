test_that("tsfvit04", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfvit04.R"), "tsfvit04.rtf")
})
