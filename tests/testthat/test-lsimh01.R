test_that("lsimh01", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsimh01.R"), "lsimh01.rtf")
})
