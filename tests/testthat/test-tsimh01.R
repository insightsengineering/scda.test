test_that("tsimh01", {
  expect_snapshot_file(write_test_rtf_for("tsimh01.R"), "tsimh01.rtf")
})
