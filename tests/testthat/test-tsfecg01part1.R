test_that("tsfecg01", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfecg01.R", part_num = 1, total_parts = 2), "tsfecg01part1of2.rtf")
})
