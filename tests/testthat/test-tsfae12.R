test_that("tsfae12", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae12.R"), "tsfae12.rtf")
})
