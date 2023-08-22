# Test single variant for CMT02_PT

adsl <- adsl_raw
adcm <- adcm_raw

adcm$CMSEQ <- as.factor(adcm$CMSEQ)

adsl <- df_explicit_na(adsl, na_level = "No Coding Available")
adcm <- df_explicit_na(adcm, na_level = "No Coding Available")

adsl <- adsl %>%
  filter(SAFFL == "Y")

adcm <- adcm %>% filter(ATIREL == "CONCOMITANT")

testthat::test_that("CMT02_PT default variant is produced correctly", {
  split_fun <- drop_split_levels

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
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
    append_topleft(paste("\nOther Treatment"))

  result <- build_table(lyt = lyt, df = adcm, alt_counts_df = adsl) %>%
    sort_at_path(
      path = c("CMDECOD"),
      scorefun = score_occurrences
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
