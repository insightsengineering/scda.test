test_that("lsfdth01", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsfdth01.R"), "lsfdth01.rtf")
})
