test_that("tsfae11", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae11.R"), "tsfae11.rtf")
})
