test_that("tsiex09", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsiex09.R"), "tsiex09.rtf")
})
