test_that("tsfae24fpart3of3", {
  expect_snapshot_file(write_test_rtf_for("tsfae24f.R", part_num = 3, total_parts = 3), "tsfae24fpart3of3.rtf")
})
