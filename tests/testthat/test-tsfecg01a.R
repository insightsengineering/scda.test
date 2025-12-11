test_that("tsfecg01a", {
  expect_snapshot_file(write_test_rtf_for("tsfecg01a.R", part_num = 1, total_parts = 3), "tsfecg01apart1of3.rtf")
})
