test_that("lsfae06b", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsfae06b.R"), "lsfae06b.rtf")
})
