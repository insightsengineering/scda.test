test_that("lsiex02", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsiex02.R"), "lsiex02.rtf")
})
