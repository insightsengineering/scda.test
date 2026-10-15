test_that("tsids02", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsids02.R"), "tsids02.rtf")
})
