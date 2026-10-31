test_that("tsflab08", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsflab08.R"), "tsflab08.rtf")
})
