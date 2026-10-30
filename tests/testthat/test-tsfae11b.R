test_that("tsfae11bpart1of4", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae11b.R", part_num = 1, total_parts = 4), "tsfae11bpart1of4.rtf")
})

# test_that("tsfae11bpart2of4", {
#   skip_if_not_installed("envsetup")

#   expect_snapshot_file(write_test_rtf_for("tsfae11b.R", part_num = 2, total_parts = 4), "tsfae11bpart2of4.rtf")
# })

# test_that("tsfae11bpart3of4", {
#   skip_if_not_installed("envsetup")

#   expect_snapshot_file(write_test_rtf_for("tsfae11b.R", part_num = 3, total_parts = 4), "tsfae11bpart3of4.rtf")
# })

# test_that("tsfae11bpart4of4", {
#   skip_if_not_installed("envsetup")

#   expect_snapshot_file(write_test_rtf_for("tsfae11b.R", part_num = 4, total_parts = 4), "tsfae11bpart4of4.rtf")
# })
