test_that("tsfae12apart1of4", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae12a.R", part_num = 1, total_parts = 4), "tsfae12apart1of4.rtf")
})

# test_that("tsfae12apart2of4", {
#   skip_if_not_installed("envsetup")

#   expect_snapshot_file(write_test_rtf_for("tsfae12a.R", part_num = 2, total_parts = 4), "tsfae12apart2of4.rtf")
# })

# test_that("tsfae12apart3of4", {
#   skip_if_not_installed("envsetup")

#   expect_snapshot_file(write_test_rtf_for("tsfae12a.R", part_num = 3, total_parts = 4), "tsfae12apart3of4.rtf")
# })

# test_that("tsfae12apart4of4", {
#   skip_if_not_installed("envsetup")

#   expect_snapshot_file(write_test_rtf_for("tsfae12a.R", part_num = 4, total_parts = 4), "tsfae12apart4of4.rtf")
# })
