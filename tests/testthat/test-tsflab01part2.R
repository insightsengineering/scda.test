test_that("tsflab01", {
  expect_snapshot_file(
    write_test_rtf_for("tsflab01.R", part_num = 2, total_parts = 2),
    "tsflab01part2of2.rtf"
  )
})
