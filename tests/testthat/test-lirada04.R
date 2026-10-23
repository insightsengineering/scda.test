test_that("lirada04", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lirada04.R", rtf_name = "lirada04.rtf"), "lirada04.rtf")
})
