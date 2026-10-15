test_that("tirada03titer", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tirada03titer.R", rtf_name = "tirada03titer.rtf"), "tirada03titer.rtf")
})
