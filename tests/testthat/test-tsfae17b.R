test_that("tsfae17b", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae17b.R"), "tsfae17b.rtf")
})
