test_that("tsflab02b", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsflab02b.R"), "tsflab02b.rtf")
})
