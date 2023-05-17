adsl <- adsl_raw
adex <- adex_raw

# Ensure character variables are converted to factors and empty strings and NAs are explicit missing levels.
adsl <- df_explicit_na(adsl)
adex <- df_explicit_na(adex)

# Simulate ADEX records with PARAMCD == "TDURD" as they are not in sample scda dataset.
set.seed(1, kind = "Mersenne-Twister")
adex2 <- adex %>%
  distinct(USUBJID, .keep_all = TRUE) %>%
  mutate(
    PARAMCD = "TDURD",
    PARAM = "Overall duration (days)",
    AVAL = sample(x = seq(1, 200), size = n(), replace = TRUE)
  ) %>%
  bind_rows(adex)

# Now pre-processing steps are carried out.
anl <- adex2 %>%
  filter(
    PARAMCD == "TDURD",
    PARCAT2 == "Drug A",
    SAFFL == "Y"
  ) %>%
  mutate(
    aval_months = day2month(AVAL),
    aval_months_cat = factor(case_when(
      aval_months < 1 ~ "< 1 month",
      aval_months >= 1 & aval_months < 3 ~ "1 to <3 months",
      aval_months >= 3 & aval_months < 6 ~ "3 to <6 months",
      TRUE ~ ">=6 months"
    ), levels = c("< 1 month", "1 to <3 months", "3 to <6 months", ">=6 months"))
  )

adsl_f <- adsl %>% filter(adsl$SAFFL == "Y")

testthat::test_that("RMPT01 is produced correctly", {
  lyt <- basic_table(
    title = "Duration of Exposure: Safety-Evaluable Patients",
    main_footer = "* Patient Time is the sum of exposure across all patients in days.",
    show_colcounts = TRUE
  ) %>%
    summarize_patients_exposure_in_cols(
      var = "AVAL", col_split = TRUE,
      .labels = c(n_patients = "Number of Patients", sum_exposure = "Patient Time*"),
      custom_label = "Total Number of Patients and Patient Time"
    ) %>%
    analyze_patients_exposure_in_cols(
      var = "aval_months_cat",
      col_split = FALSE
    ) %>%
    append_topleft(c("", "Duration of exposure"))

  result <- build_table(lyt, df = anl, alt_counts_df = adsl_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
