Sys.setenv(JUNCO_DISABLE_VALIDATION=TRUE)
test_that("tsfae19a", {
  expect_snapshot_file(write_test_rtf_for("tsfae19a.R"), "tsfae19a.rtf")
})
Sys.setenv(JUNCO_DISABLE_VALIDATION=FALSE)

