test_that("tsflab04b", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsflab04b.R"), "tsflab04b.rtf")
})
