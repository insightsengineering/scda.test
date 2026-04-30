test_that("tsfae03", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae03.R"), "tsfae03.rtf")
})
