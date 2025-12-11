test_that("tsfae21apart3of4", {
  expect_snapshot_file(write_test_rtf_for("tsfae21a.R", part_num = 3, total_parts = 4), "tsfae21apart3of4.rtf")
})