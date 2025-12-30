test_that("tsfecg03", {
  expect_snapshot_file(write_test_rtf_for("tsfecg03.R"), "tsfecg03.rtf")
})
