test_that("lsidev02", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsidev02.R"), "lsidev02.rtf")
})
