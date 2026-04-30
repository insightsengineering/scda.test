test_that("tsicm01", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsicm01.R"), "tsicm01.rtf")
})
