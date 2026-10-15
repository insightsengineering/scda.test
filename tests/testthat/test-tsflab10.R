test_that("tsflab10", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsflab10.R"), "tsflab10.rtf")
})
