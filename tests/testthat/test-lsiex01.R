test_that("lsiex01", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsiex01.R"), "lsiex01.rtf")
})
