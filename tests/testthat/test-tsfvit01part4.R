test_that("tsfvit01", {
  expect_snapshot_file(
    write_test_rtf_for("tsfvit01.R", part_num = 4, total_parts = 4),
    "tsfvit01part4of4.rtf"
  )
})
