test_that("lsfae03", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsfae03.R"), "lsfae03.rtf")
})
