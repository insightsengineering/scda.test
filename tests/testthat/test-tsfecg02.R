test_that("tsfecg02", {
  expect_snapshot_file(write_test_rtf_for("tsfecg02.R"), "tsfecg02.rtf")
})

