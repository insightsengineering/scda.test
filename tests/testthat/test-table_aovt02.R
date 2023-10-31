adsl <- adsl_raw
adqs <- adqs_raw

# Ensure character variables are converted to factors and empty strings and NAs are explicit missing levels.
adsl <- df_explicit_na(adsl)
adqs <- df_explicit_na(adqs)

adqs_single <- adqs %>%
  filter(
    AVISIT == "WEEK 1 DAY 8",
    PARAMCD == "FKSI-FWB"
  ) %>%
  mutate(CHG = ifelse(BMEASIFL == "Y", CHG, NA))

testthat::test_that("AOVT02 is produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARMCD", ref_group = "ARM A", split_fun = ref_group_position("first")) %>%
    append_varlabels(adqs_single, "PARAM") %>%
    summarize_ancova(
      vars = "CHG",
      variables = list(arm = "ARMCD", covariates = NULL),
      conf_level = 0.95,
      var_labels = "Unadjusted comparison",
      .labels = c(lsmean = "Mean", lsmean_diff = "Difference in Means"),
      table_names = "unadjusted"
    ) %>%
    summarize_ancova(
      vars = "CHG",
      variables = list(arm = "ARMCD", covariates = c("BASE", "STRATA1")),
      conf_level = 0.95,
      var_labels = "Adjusted comparison (covariates BASE and STRATA1)",
      table_names = "adjusted"
    )

  result <- build_table(
    lyt = lyt,
    df = adqs_single,
    alt_counts_df = adsl
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
