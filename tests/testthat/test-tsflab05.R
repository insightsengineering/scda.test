test_that("tsflab05", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsflab05.R"), "tsflab05.rtf")
})
