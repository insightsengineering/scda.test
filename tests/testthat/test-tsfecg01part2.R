test_that("tsfecg01", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfecg01.R", part_num = 2, total_parts = 2), "tsfecg01part2of2.rtf")
})
