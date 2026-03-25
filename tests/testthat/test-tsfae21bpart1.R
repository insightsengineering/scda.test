test_that("tsfae21bpart1of4", {
  expect_snapshot_file(write_test_rtf_for("tsfae21b.R", part_num = 1, total_parts = 4), "tsfae21bpart1of4.rtf")
})
