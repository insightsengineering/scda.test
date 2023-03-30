adsl <- adsl_raw
adaette <- adaette_raw

# Ensure character variables are converted to factors and empty strings and NAs are explicit missing levels.
adsl <- df_explicit_na(adsl)
adaette <- df_explicit_na(adaette)

# Create analysis dataset anl from the 2 parameters AEREPTTE & AETOT1
anl_events <- adaette %>%
  filter(PARAMCD == "AETOT1") %>%
  select(USUBJID, ARM, ARMCD, n_events = AVAL) %>%
  mutate(n_events = as.integer(n_events))

anl_tte <- adaette %>%
  filter(PARAMCD == "AEREPTTE") %>%
  select(USUBJID, ARM, ARMCD, AVAL)

anl <- full_join(anl_tte, anl_events, by = c("USUBJID", "ARM", "ARMCD"))

testthat::test_that("AET05_ALL variant 1 is produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    estimate_incidence_rate(
      vars = "AVAL",
      n_events = "n_events",
      control = control_incidence_rate(time_unit_output = 100)
    )

  result <- build_table(lyt, anl, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET05_ALL variant 2 is produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    estimate_incidence_rate(
      vars = "AVAL",
      n_events = "n_events",
      control = control_incidence_rate(conf_type = "exact", time_unit_output = 100)
    )

  result <- build_table(lyt, anl, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
