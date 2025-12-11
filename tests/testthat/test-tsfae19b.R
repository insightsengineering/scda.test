Sys.setenv(JUNCO_DISABLE_VALIDATION=TRUE)
test_that("tsfae19b", {
  expect_snapshot_file(write_test_rtf_for("tsfae19b.R"), "tsfae19b.rtf")
})
Sys.setenv(JUNCO_DISABLE_VALIDATION=FALSE)

