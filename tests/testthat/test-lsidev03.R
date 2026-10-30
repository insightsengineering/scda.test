test_that("lsidev03", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsidev03.R"), "lsidev03.rtf")
})
