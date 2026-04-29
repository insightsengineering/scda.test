test_that("tsicm06", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsicm06.R"), "tsicm06.rtf")
})
