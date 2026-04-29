test_that("tsflab01a", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(
    write_test_rtf_for("tsflab01a.R", part_num = 3, total_parts = 3),
    "tsflab01apart3of3.rtf"
  )
})
