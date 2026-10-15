test_that("tsids02b", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsids02b.R"), "tsids02b.rtf")
})
