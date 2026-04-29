test_that("tsfae15", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae15.R"), "tsfae15.rtf")
})
