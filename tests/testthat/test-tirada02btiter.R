test_that("tirada02btiter", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tirada02btiter.R", rtf_name = "tirada02btiter.rtf"), "tirada02btiter.rtf")
})
