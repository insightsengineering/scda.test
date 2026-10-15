test_that("tsflab03", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsflab03.R"), "tsflab03.rtf")
})
