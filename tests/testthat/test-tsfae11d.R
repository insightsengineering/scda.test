test_that("tsfae11d", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae11d.R"), "tsfae11d.rtf")
})
