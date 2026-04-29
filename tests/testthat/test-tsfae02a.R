test_that("tsfae02a", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae02a.R"), "tsfae02a.rtf")
})
