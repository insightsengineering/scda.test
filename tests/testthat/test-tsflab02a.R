test_that("tsflab02a", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsflab02a.R"), "tsflab02a.rtf")
})
