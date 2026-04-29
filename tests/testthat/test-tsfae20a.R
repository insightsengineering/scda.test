test_that("tsfae20a", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae20a.R"), "tsfae20a.rtf")
})
