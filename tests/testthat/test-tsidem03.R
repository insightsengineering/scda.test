test_that("tsidem03", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsidem03.R"), "tsidem03.rtf")
})
