test_that("tsfae24b", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae24b.R"), "tsfae24b.rtf")
})
