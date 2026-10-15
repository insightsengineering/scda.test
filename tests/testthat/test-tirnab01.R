test_that("tirnab01", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tirnab01.R", rtf_name = "tirnab01.rtf"), "tirnab01.rtf")
})
