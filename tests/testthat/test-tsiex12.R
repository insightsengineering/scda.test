test_that("tsiex12", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsiex12.R"), "tsiex12.rtf")
})
