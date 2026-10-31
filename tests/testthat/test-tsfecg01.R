test_that("tsfecg01", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfecg01.R"), "tsfecg01.rtf")
})
