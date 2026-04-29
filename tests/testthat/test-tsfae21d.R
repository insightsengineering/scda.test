test_that("tsfae21d", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae21d.R"), "tsfae21d.rtf")
})
