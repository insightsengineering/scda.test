test_that("lsfae06", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsfae06.R"), "lsfae06.rtf")
})
