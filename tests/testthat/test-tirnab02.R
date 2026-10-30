test_that("tirnab02", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tirnab02.R", rtf_name = "tirnab02.rtf"), "tirnab02.rtf")
})
