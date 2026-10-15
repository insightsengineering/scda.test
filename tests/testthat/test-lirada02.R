test_that("lirada02", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lirada02.R", rtf_name = "lirada02.rtf"), "lirada02.rtf")
})
