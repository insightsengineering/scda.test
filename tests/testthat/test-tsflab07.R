test_that("tsflab07", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsflab07.R"), "tsflab07.rtf")
})
