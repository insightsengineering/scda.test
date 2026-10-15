test_that("tsfae10b", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae10b.R"), "tsfae10b.rtf")
})
