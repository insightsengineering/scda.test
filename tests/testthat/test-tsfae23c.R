test_that("tsfae23c", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae23c.R"), "tsfae23c.rtf")
})
