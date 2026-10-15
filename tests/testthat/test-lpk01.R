test_that("lpk01", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lpk01.R"), "lpk01.rtf")
})
