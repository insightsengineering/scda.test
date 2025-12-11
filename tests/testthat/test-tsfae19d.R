Sys.setenv(JUNCO_DISABLE_VALIDATION=TRUE)
test_that("tsfae19d", {
  expect_snapshot_file(write_test_rtf_for("tsfae19d.R"), "tsfae19d.rtf")
})
Sys.setenv(JUNCO_DISABLE_VALIDATION=FALSE)