test_that("tsflab01a", {
  expect_snapshot_file(write_test_rtf_for("tsflab01a.R", part_num = 1, total_parts = 3), "tsflab01apart1of3.rtf")
})
