test_that("lsfae05b", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsfae05b.R"), "lsfae05b.rtf")
})
