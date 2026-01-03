test_that("tsfvit01a", {
  expect_snapshot_file(
    write_test_rtf_for("tsfvit01a.R", part_num = 1, total_parts = 4),
    "tsfvit01apart1of4.rtf"
  )
})
