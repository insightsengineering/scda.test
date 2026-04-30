test_that("lsiex03", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsiex03.R"), "lsiex03.rtf")
})
