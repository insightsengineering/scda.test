test_that("lsfae06a", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsfae06a.R"), "lsfae06a.rtf")
})
