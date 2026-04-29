test_that("tsfae17d", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae17d.R"), "tsfae17d.rtf")
})
