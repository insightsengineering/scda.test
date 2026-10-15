test_that("tsfae07c", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae07c.R"), "tsfae07c.rtf")
})
