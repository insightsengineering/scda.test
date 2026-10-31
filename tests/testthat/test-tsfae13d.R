test_that("tsfae13d", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae13d.R"), "tsfae13d.rtf")
})
