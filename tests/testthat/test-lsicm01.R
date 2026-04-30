test_that("lsicm01", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsicm01.R"), "lsicm01.rtf")
})
