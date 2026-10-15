test_that("tirada01", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tirada01.R", rtf_name = "tirada01.rtf"), "tirada01.rtf")
})
