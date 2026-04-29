test_that("tsiex04", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsiex04.R"), "tsiex04.rtf")
})
