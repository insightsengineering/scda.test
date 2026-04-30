test_that("lsfae02", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsfae02.R"), "lsfae02.rtf")
})
