test_that("tirada03", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tirada03.R", rtf_name = "tirada03.rtf"), "tirada03.rtf")
})
