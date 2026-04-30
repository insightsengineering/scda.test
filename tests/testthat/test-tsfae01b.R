test_that("tsfae01b", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae01b.R"), "tsfae01b.rtf")
})
