test_that("tsfae12dpart1of2", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae12d.R", part_num = 1, total_parts = 2), "tsfae12dpart1of2.rtf")
})

# test_that("tsfae12dpart2of2", {
#   skip_if_not_installed("envsetup")

#   expect_snapshot_file(write_test_rtf_for("tsfae12d.R", part_num = 2, total_parts = 2), "tsfae12dpart2of2.rtf")
# })
