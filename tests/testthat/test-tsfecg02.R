test_that("tsfecg02", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfecg02.R"), "tsfecg02.rtf")
})
