test_that("lsfecg01", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsfecg01.R"), "lsfecg01.rtf")
})
