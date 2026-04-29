test_that("tsicm05", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsicm05.R"), "tsicm05.rtf")
})
