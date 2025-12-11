test_that("tsfae21apart2of4", {
  expect_snapshot_file(write_test_rtf_for("tsfae21a.R", part_num = 2, total_parts = 4), "tsfae21apart2of4.rtf")
})