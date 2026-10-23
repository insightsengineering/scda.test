test_that("lsidev01", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsidev01.R"), "lsidev01.rtf")
})
