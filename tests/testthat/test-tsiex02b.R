test_that("tsiex02b", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsiex02b.R"), "tsiex02b.rtf")
})
