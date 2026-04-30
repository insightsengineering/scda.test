test_that("tsidem01", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsidem01.R"), "tsidem01.rtf")
})
