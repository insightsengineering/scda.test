test_that("tsfae21bpart2of4", {
  expect_snapshot_file(write_test_rtf_for("tsfae21b.R", part_num = 2, total_parts = 4), "tsfae21bpart2of4.rtf")
})