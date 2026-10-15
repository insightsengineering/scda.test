test_that("tirnab03", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tirnab03.R", rtf_name = "tirnab03.rtf"), "tirnab03.rtf")
})
