test_that("tpk02part2of2", {
  expect_snapshot_file(write_test_rtf_for("tpk02.R", part_num = 2, total_parts = 2), "tpk02part2of2.rtf")
})