test_that("tsfae22bpart1of2", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae22b.R", part_num = 1, total_parts = 2), "tsfae22bpart1of2.rtf")
})
