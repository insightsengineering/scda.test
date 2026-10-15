test_that("tsfae13b", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae13b.R"), "tsfae13b.rtf")
})
