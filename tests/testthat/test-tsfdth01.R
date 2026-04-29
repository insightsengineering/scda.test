test_that("tsfdth01", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfdth01.R"), "tsfdth01.rtf")
})
