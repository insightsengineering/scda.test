Sys.setenv(JUNCO_DISABLE_VALIDATION = TRUE)
test_that("tsfae19c", {
  skip_if_not_installed("envsetup")

  expect_snapshot_file(write_test_rtf_for("tsfae19c.R"), "tsfae19c.rtf")
})
Sys.setenv(JUNCO_DISABLE_VALIDATION = FALSE)
