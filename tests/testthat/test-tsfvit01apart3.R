test_that("tsfvit01a", {
  expect_snapshot_file(
    write_test_rtf_for("tsfvit01a.R", part_num = 3, total_parts = 4),
    "tsfvit01apart3of4.rtf"
  )
})
