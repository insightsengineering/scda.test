test_that("lsids02", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsids02.R"), "lsids02.rtf")
})
