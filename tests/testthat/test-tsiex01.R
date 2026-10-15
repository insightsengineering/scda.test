test_that("tsiex01", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsiex01.R"), "tsiex01.rtf")
})
