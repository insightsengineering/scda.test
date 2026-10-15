test_that("tsfvit01", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfvit01.R"), "tsfvit01.rtf")
})
