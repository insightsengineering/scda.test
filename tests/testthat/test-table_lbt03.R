# Tests the single variant for LBT03

adsl <- adsl_pharmaverse
adlb <- adlb_pharmaverse

testthat::test_that("LBT03 default variant is produced correctly", {
  adlb_f <- adlb %>%
    dplyr::filter(AVISIT %in% c("Baseline", "Week 2", "Week 4") & PARAMCD == "ALT") %>%
    dplyr::mutate(
      ABLFL = ifelse(AVISIT == "Baseline", "Y", ""), # original adlb_pharmaverse ABLFL is unreliable
      ABLFLL = ABLFL == "Y"
    )

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    split_rows_by("AVISIT") %>%
    summarize_change(
      "CHG",
      variables = list(value = "AVAL", baseline_flag = "ABLFLL"),
      na.rm = TRUE
    ) %>%
    build_table(
      df = adlb_f,
      alt_counts_df = adsl
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
