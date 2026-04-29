test_that("tsfae05a", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae05a.R"), "tsfae05a.rtf")
})
