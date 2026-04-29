test_that("lsfvit02", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsfvit02.R"), "lsfvit02.rtf")
})
