test_that("tirada02titer", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tirada02titer.R", rtf_name = "tirada02titer.rtf"), "tirada02titer.rtf")
})
