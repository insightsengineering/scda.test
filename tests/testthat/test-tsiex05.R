test_that("tsiex05", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsiex05.R"), "tsiex05.rtf")
})
