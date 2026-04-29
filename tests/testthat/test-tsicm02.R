test_that("tsicm02", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsicm02.R"), "tsicm02.rtf")
})
