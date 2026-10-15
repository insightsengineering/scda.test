test_that("tsfae07a", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae07a.R"), "tsfae07a.rtf")
})
