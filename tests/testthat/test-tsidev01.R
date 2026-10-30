test_that("tsidev01", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsidev01.R"), "tsidev01.rtf")
})
