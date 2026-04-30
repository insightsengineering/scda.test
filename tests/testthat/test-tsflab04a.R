test_that("tsflab04a", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsflab04a.R"), "tsflab04a.rtf")
})
