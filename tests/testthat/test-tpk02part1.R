test_that("tpk02part1of2", {
  expect_snapshot_file(write_test_rtf_for("tpk02.R", part_num = 1, total_parts = 2), "tpk02part1of2.rtf")
})
