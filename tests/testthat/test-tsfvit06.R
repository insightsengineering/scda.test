test_that("tsfvit06", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfvit06.R"), "tsfvit06.rtf")
})
