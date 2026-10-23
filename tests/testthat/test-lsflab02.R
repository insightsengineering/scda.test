test_that("lsflab02", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsflab02.R"), "lsflab02.rtf")
})
