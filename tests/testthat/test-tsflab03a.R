test_that("tsflab03a", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsflab03a.R"), "tsflab03a.rtf")
})
