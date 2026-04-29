test_that("tsiex08", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsiex08.R"), "tsiex08.rtf")
})
