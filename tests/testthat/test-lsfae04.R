test_that("lsfae04", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsfae04.R"), "lsfae04.rtf")
})
