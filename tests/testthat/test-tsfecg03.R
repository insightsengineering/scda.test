test_that("tsfecg03", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfecg03.R"), "tsfecg03.rtf")
})
