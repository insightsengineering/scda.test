test_that("lsfecg02", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsfecg02.R"), "lsfecg02.rtf")
})
