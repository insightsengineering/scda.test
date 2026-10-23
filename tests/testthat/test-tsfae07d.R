test_that("tsfae07d", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae07d.R"), "tsfae07d.rtf")
})
