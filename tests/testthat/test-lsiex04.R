test_that("lsiex04", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsiex04.R"), "lsiex04.rtf")
})
