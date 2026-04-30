test_that("tsfae23d", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae23d.R"), "tsfae23d.rtf")
})
