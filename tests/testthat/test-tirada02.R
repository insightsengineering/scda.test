test_that("tirada02", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tirada02.R", rtf_name = "tirada02.rtf"), "tirada02.rtf")
})
