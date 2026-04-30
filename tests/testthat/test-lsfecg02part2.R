test_that("lsfecg02part2of3", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsfecg02.R", part_num = 2, total_parts = 3), "lsfecg02part2of3.rtf")
})
