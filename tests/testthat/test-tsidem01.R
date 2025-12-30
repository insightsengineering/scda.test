test_that("tsidem01", {
  expect_snapshot_file(write_test_rtf_for("tsidem01.R"), "tsidem01.rtf")
})
