test_that("tsiex03b", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsiex03b.R"), "tsiex03b.rtf")
})
