test_that("tsfecg01", {
  expect_snapshot_file(write_test_rtf_for("tsfecg01.R", part_num = 2, total_parts = 2), "tsfecg01part2of2.rtf")
})
