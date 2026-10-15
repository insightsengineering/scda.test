test_that("tsiex06", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsiex06.R"), "tsiex06.rtf")
})
