test_that("tsfae20b", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae20b.R"), "tsfae20b.rtf")
})
