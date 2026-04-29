test_that("tsids01", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsids01.R"), "tsids01.rtf")
})
