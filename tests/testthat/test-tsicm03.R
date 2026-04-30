test_that("tsicm03", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsicm03.R"), "tsicm03.rtf")
})
