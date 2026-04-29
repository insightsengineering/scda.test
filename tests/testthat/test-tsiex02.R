test_that("tsiex02", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsiex02.R"), "tsiex02.rtf")
})
