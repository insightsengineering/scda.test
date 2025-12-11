test_that("tsfae22apart4of4", {
  expect_snapshot_file(write_test_rtf_for("tsfae22a.R", part_num = 4, total_parts = 4), "tsfae22apart4of4.rtf")
})