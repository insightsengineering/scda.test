test_that("tsfae21apart3of4", {
  skip_if_not_installed("envsetup")
  skip_if_not_installed("tern")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("rtables")
  skip_if_not_installed("rlistings")
  skip_if_not_installed("stringi")
  expect_snapshot_file(write_test_rtf_for("tsfae21a.R", part_num = 3, total_parts = 4), "tsfae21apart3of4.rtf")
})
