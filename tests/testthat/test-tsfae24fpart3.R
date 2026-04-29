test_that("tsfae24fpart3of3", {
  skip_if_not_installed("envsetup")
  skip_if_not_installed("tern")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("rtables")
  skip_if_not_installed("rlistings")
  skip_if_not_installed("stringi")
  expect_snapshot_file(write_test_rtf_for("tsfae24f.R", part_num = 3, total_parts = 3), "tsfae24fpart3of3.rtf")
})
