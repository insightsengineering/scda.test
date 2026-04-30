test_that("tsfvit03", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfvit03.R"), "tsfvit03.rtf")
})
