test_that("tsfae22apart1of4", {
  expect_snapshot_file(write_test_rtf_for("tsfae22a.R", part_num = 1, total_parts = 4), "tsfae22apart1of4.rtf")
})
