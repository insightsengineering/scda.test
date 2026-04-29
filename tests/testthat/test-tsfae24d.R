test_that("tsfae24d", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae24d.R"), "tsfae24d.rtf")
})
