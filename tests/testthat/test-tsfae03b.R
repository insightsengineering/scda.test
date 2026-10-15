test_that("tsfae03b", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae03b.R"), "tsfae03b.rtf")
})
