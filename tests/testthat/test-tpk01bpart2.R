test_that("tpk01bpart2of2", {
  expect_snapshot_file(write_test_rtf_for("tpk01b.R", part_num = 2, total_parts = 2), "tpk01bpart2of2.rtf")
})
