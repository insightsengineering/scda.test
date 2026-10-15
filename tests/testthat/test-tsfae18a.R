test_that("tsfae18a", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae18a.R"), "tsfae18a.rtf")
})
