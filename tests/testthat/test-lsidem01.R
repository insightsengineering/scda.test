test_that("lsidem01", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("lsidem01.R"), "lsidem01.rtf")
})
