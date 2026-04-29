test_that("lsids01", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsids01.R"), "lsids01.rtf")
})
