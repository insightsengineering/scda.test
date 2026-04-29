test_that("tsicm07", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsicm07.R"), "tsicm07.rtf")
})
