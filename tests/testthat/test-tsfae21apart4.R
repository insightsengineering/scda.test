test_that("tsfae21apart4of4", {
  expect_snapshot_file(write_test_rtf_for("tsfae21a.R", part_num = 4, total_parts = 4), "tsfae21apart4of4.rtf")
})