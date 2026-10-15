test_that("lirada01", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lirada01.R", rtf_name = "lirada01.rtf"), "lirada01.rtf")
})
