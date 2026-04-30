test_that("tpk01a", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tpk01a.R"), "tpk01a.rtf")
})
