test_that("tpk03", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tpk03.R"), "tpk03.rtf")
})
