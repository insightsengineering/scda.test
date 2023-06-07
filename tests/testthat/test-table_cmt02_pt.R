# Test single variant for CMT02_PT

adsl <- adsl_raw
adcm <- adcm_raw

testthat::test_that("CMT02_PT default variant is produced correctly", {
  adcm <- adcm %>%
    dplyr::mutate(
      CMSEQ = as.factor(CMSEQ)
    )

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    add_overall_col("All Patients") %>%
    analyze_num_patients(
      vars = "USUBJID",
      count_by = "CMSEQ",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one treatment",
        nonunique = "Total number of treatments"
      ),
      show_labels = "hidden"
    ) %>%
    count_occurrences(vars = "CMDECOD") %>%
    append_varlabels(adcm, "CMDECOD", indent = 0)

  result <- build_table(
    lyt = lyt,
    df = adcm,
    alt_counts_df = adsl
  ) %>%
    sort_at_path(
      path = c("CMDECOD"),
      scorefun = score_occurrences
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
