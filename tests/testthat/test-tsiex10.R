test_that("tsiex10", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsiex10.R"), "tsiex10.rtf")
})
