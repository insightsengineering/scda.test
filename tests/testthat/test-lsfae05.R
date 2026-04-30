test_that("lsfae05", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsfae05.R"), "lsfae05.rtf")
})
