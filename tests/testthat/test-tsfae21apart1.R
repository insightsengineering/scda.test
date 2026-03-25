test_that("tsfae21apart1of4", {
  expect_snapshot_file(write_test_rtf_for("tsfae21a.R", part_num = 1, total_parts = 4), "tsfae21apart1of4.rtf")
})
