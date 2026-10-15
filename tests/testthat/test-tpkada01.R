test_that("tpkada01", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tpkada01.R"), "tpkada01.rtf")
})
