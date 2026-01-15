test_that("tsfecg01a", {
  expect_snapshot_file(write_test_rtf_for("tsfecg01a.R", part_num = 3, total_parts = 3), "tsfecg01apart3of3.rtf")
})
