test_that("tsflab02b", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsflab02b.R", rtf_name = "tsflab02b.rtf"), "tsflab02b.rtf")
})
