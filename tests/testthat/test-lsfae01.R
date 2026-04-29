test_that("lsfae01", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsfae01.R"), "lsfae01.rtf")
})
