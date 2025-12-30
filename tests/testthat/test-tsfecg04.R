test_that("tsfecg04", {
  expect_snapshot_file(write_test_rtf_for("tsfecg04.R"), "tsfecg04.rtf")
})
