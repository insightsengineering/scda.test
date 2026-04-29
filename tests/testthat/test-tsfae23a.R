test_that("tsfae23a", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae23a.R"), "tsfae23a.rtf")
})
