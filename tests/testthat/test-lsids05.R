test_that("lsids05", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsids05.R"), "lsids05.rtf")
})
