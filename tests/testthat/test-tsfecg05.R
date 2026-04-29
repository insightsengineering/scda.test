test_that("tsfecg05", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfecg05.R"), "tsfecg05.rtf")
})
