test_that("tsfae03a", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae03a.R"), "tsfae03a.rtf")
})
