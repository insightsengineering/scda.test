test_that("tsiex03", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsiex03.R"), "tsiex03.rtf")
})
