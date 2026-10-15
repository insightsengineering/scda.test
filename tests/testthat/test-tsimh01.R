test_that("tsimh01", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsimh01.R"), "tsimh01.rtf")
})
