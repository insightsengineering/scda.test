test_that("tsfvit01", {
  skip_if_not_installed("envsetup")
  skip_if_not_installed("tern")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("rtables")
  skip_if_not_installed("rlistings")
  skip_if_not_installed("stringi")
  expect_snapshot_file(
    write_test_rtf_for("tsfvit01.R", part_num = 4, total_parts = 4),
    "tsfvit01part4of4.rtf"
  )
})
