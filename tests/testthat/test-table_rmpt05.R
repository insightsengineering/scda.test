adsl <- adsl_raw
adex <- adex_raw

# Ensure character variables are converted to factors and empty strings and NAs are explicit missing levels.
adsl <- df_explicit_na(adsl)
adex <- df_explicit_na(adex)

# Simulate ADEX records with PARAMCD == "TDURD" as they are not in the sample dataset.
set.seed(1)
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
  mutate()

adsl_f <- adsl %>% filter(adsl$SAFFL == "Y")

testthat::test_that("RMPT05 is produced correctly", {
  lyt <- basic_table(
    title = "Extent of Exposure by Race: Safety-Evaluable Patients",
    main_footer = "* Person time is the sum of exposure across all patients in unit: days.",
    show_colcounts = TRUE
  ) %>%
    summarize_patients_exposure_in_cols(
      var = "AVAL", col_split = TRUE,
      .labels = c(n_patients = "Patients", sum_exposure = "Person time*")
    ) %>%
    analyze_patients_exposure_in_cols(
      var = "RACE",
      col_split = FALSE
    ) %>%
    append_topleft(c("", obj_label(adex$RACE)))

  result <- build_table(lyt, df = anl, alt_counts_df = adsl_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
