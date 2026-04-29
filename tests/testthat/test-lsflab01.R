test_that("lsflab01", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsflab01.R"), "lsflab01.rtf")
})
