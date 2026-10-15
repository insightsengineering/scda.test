test_that("tsfae07e", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae07e.R"), "tsfae07e.rtf")
})
