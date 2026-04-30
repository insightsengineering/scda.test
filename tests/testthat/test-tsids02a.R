test_that("tsids02a", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsids02a.R"), "tsids02a.rtf")
})
