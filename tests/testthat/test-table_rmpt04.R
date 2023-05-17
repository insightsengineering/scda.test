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
  )

adsl_f <- adsl %>% filter(adsl$SAFFL == "Y")

testthat::test_that("RMPT04 variant 1 is produced correctly", {
  lyt <- basic_table(
    title = "Extent of Exposure by Ethnic Origin: Safety-Evaluable Patients",
    main_footer = "* Patient Time is the sum of exposure across all patients in days.",
    show_colcounts = TRUE
  ) %>%
    summarize_patients_exposure_in_cols(
      var = "AVAL", col_split = TRUE,
      .labels = c(n_patients = "Number of Patients", sum_exposure = "Patient Time*"),
      custom_label = "Total Number of Patients and Patient Time"
    ) %>%
    analyze_patients_exposure_in_cols(
      var = "ETHNIC",
      col_split = FALSE
    ) %>%
    append_topleft(c("", "Ethnicity"))

  result <- build_table(lyt, df = anl, alt_counts_df = adsl_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
