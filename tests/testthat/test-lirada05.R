test_that("lirada05", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lirada05.R", rtf_name = "lirada05.rtf"), "lirada05.rtf")
})
