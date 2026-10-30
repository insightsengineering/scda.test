test_that("tsfae18b", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae18b.R"), "tsfae18b.rtf")
})
