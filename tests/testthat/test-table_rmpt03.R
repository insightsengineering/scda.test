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
    AGEGR1 = factor(ifelse(AGE < 65, "<65", ">=65")),
    AGEGR2 = factor(case_when(
      AGE < 18 ~ "< 18",
      AGE >= 18 & AGE <= 40 ~ "18 - 40",
      AGE > 40 & AGE <= 64 ~ "41 - 64",
      TRUE ~ ">=65"
    ), levels = c("< 18", "18 - 40", "41 - 64", ">=65")),
    SEX = factor(case_when(
      SEX == "F" ~ "Female",
      SEX == "M" ~ "Male"
    ))
  )

adsl_f <- adsl %>%
  filter(adsl$SAFFL == "Y") %>%
  mutate(SEX = factor(case_when(
    SEX == "F" ~ "Female",
    SEX == "M" ~ "Male"
  )))

testthat::test_that("RMPT03 variant 1 is produced correctly", {
  lyt <- basic_table(
    title = "Extent of Exposure by Age Group and Gender: Safety-Evaluable Patients",
    main_footer = "* Patient Time is the sum of exposure across all patients in days."
  ) %>%
    split_cols_by("SEX", split_fun = add_overall_level("Total", first = FALSE)) %>%
    summarize_patients_exposure_in_cols(
      var = "AVAL", col_split = TRUE,
      .labels = c(n_patients = "Number of Patients", sum_exposure = "Patient Time*"),
      custom_label = "Total Number of Patients and Patient Time"
    ) %>%
    split_rows_by("AGEGR1", label_pos = "topleft", split_label = "Age group (years)") %>%
    summarize_patients_exposure_in_cols(
      var = "AVAL",
      col_split = FALSE
    )

  result <- build_table(lyt, df = anl, alt_counts_df = adsl_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("RMPT03 variant 2 is produced correctly", {
  lyt <- basic_table(
    title = "Duration of Exposure: Safety-Evaluable Patients",
    main_footer = "* Patient Time is the sum of exposure across all patients in days."
  ) %>%
    split_cols_by("SEX", split_fun = add_overall_level("Total", first = FALSE)) %>%
    summarize_patients_exposure_in_cols(
      var = "AVAL", col_split = TRUE,
      .labels = c(n_patients = "Number of Patients", sum_exposure = "Patient Time*"),
      custom_label = "Total Number of Patients and Patient Time"
    ) %>%
    split_rows_by("AGEGR2",
      split_fun = drop_split_levels, # "< 18" dropped
      label_pos = "topleft", split_label = "Age group (years)"
    ) %>%
    summarize_patients_exposure_in_cols(
      var = "AVAL",
      col_split = FALSE
    )

  result <- build_table(lyt, df = anl, alt_counts_df = adsl_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
