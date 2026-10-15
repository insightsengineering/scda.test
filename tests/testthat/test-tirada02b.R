test_that("tirada02b", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tirada02b.R", rtf_name = "tirada02b.rtf"), "tirada02b.rtf")
})
