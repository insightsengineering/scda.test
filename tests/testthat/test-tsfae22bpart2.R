test_that("tsfae22bpart2of2", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae22b.R", part_num = 2, total_parts = 2), "tsfae22bpart2of2.rtf")
})
