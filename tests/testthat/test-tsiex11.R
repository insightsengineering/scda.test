test_that("tsiex11", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsiex11.R"), "tsiex11.rtf")
})
