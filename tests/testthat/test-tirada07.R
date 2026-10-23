test_that("tirada07", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tirada07.R", rtf_name = "tirada07.rtf"), "tirada07.rtf")
})
