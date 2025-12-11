test_that("tsfae16part1of2", {
  expect_snapshot_file(write_test_rtf_for("tsfae16.R", part_num = 1, total_parts = 2), "tsfae16part1of2.rtf")
})