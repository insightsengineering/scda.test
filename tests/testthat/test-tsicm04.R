test_that("tsicm04", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsicm04.R"), "tsicm04.rtf")
})
