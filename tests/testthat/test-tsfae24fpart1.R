test_that("tsfae24fpart1of3", {
  expect_snapshot_file(write_test_rtf_for("tsfae24f.R", part_num = 1, total_parts = 3), "tsfae24fpart1of3.rtf")
})
