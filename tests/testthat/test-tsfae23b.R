test_that("tsfae23b", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae23b.R"), "tsfae23b.rtf")
})
