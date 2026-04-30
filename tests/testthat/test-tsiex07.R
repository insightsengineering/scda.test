test_that("tsiex07", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsiex07.R"), "tsiex07.rtf")
})
