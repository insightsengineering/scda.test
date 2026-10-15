test_that("lirada03", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lirada03.R", rtf_name = "lirada03.rtf"), "lirada03.rtf")
})
