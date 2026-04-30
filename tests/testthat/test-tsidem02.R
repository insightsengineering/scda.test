test_that("tsidem02", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsidem02.R"), "tsidem02.rtf")
})
