test_that("tsicm08", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsicm08.R"), "tsicm08.rtf")
})
