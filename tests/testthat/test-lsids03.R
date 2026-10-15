test_that("lsids03", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsids03.R"), "lsids03.rtf")
})
