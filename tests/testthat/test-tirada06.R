test_that("tirada06", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tirada06.R", rtf_name = "tirada06.rtf"), "tirada06.rtf")
})
