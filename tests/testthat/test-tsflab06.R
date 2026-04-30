test_that("tsflab06", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsflab06.R"), "tsflab06.rtf")
})
