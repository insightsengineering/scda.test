test_that("tsfae03e", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae03e.R"), "tsfae03e.rtf")
})
