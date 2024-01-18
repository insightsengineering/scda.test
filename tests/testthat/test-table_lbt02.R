# Tests the single variant for LBT02

adsl <- adsl_pharmaverse
adlb <- adlb_pharmaverse

testthat::test_that("LBT02 default variant is produced correctly", {
  adlb <- subset(adlb, AVISIT != "SCREENING" & PARAMCD == "ALT")

  l <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    split_rows_by(var = "AVISIT") %>%
    add_colcounts() %>%
    analyze_vars(vars = "AVAL")

  result <- build_table(l, df = adlb, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
