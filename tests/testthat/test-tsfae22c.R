test_that("tsfae22c", {
  expect_snapshot_file(write_test_rtf_for("tsfae22c.R", part_num = 1, total_parts = 4), "tsfae22cpart1of4.rtf")
})
