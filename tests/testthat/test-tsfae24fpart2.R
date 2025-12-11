test_that("tsfae24fpart2of3", {
  expect_snapshot_file(write_test_rtf_for("tsfae24f.R", part_num = 2, total_parts = 3), "tsfae24fpart2of3.rtf")
})