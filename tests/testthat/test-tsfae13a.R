test_that("tsfae13a", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae13a.R"), "tsfae13a.rtf")
})
