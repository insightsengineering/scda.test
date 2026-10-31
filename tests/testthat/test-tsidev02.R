test_that("tsidev02", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsidev02.R"), "tsidev02.rtf")
})
