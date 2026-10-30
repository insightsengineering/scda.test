test_that("tsfae11apart1of2", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae11a.R", part_num = 1, total_parts = 2), "tsfae11apart1of2.rtf")
})

# test_that("tsfae11apart2of2", {
#   skip_if_not_installed("envsetup")

#   expect_snapshot_file(write_test_rtf_for("tsfae11a.R", part_num = 2, total_parts = 2), "tsfae11apart2of2.rtf")
# })
