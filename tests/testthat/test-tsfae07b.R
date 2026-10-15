test_that("tsfae07b", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae07b.R"), "tsfae07b.rtf")
})
