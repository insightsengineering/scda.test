test_that("tsfecg04", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfecg04.R"), "tsfecg04.rtf")
})
