test_that("tsfae20dpart1of3", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae20d.R", part_num = 1, total_parts = 3), "tsfae20dpart1of3.rtf")
})

# test_that("tsfae20dpart2of3", {
#   skip_if_not_installed("envsetup")

#   expect_snapshot_file(write_test_rtf_for("tsfae20d.R", part_num = 2, total_parts = 3), "tsfae20dpart2of3.rtf")
# })

# test_that("tsfae20dpart3of3", {
#   skip_if_not_installed("envsetup")

#   expect_snapshot_file(write_test_rtf_for("tsfae20d.R", part_num = 3, total_parts = 3), "tsfae20dpart3of3.rtf")
# })
