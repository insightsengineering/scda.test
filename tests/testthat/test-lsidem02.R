test_that("lsidem02", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsidem02.R"), "lsidem02.rtf")
})
