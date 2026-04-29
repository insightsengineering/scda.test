test_that("lsids04", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsids04.R"), "lsids04.rtf")
})
