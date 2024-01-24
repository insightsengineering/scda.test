adsl <- adsl_pharmaverse
adae <- adae_pharmaverse
adaette <- left_join(
  select(adsl, USUBJID, ARM, TRTSDT),
  select(filter(adae, AESEQ == 1), USUBJID, ASTDTM),
  by = "USUBJID"
) %>%
  mutate(
    PARAM = "Time to first occurrence of any adverse event",
    AVAL = as.numeric(difftime(TRTSDT, ASTDTM, unit = "days"))/365.25,
    AVALU = "YEARS",
    CNSR = ifelse(is.na(AVAL), 1, 0)
  )

adsl <- df_explicit_na(adsl)
adaette <- df_explicit_na(adaette)

anl <- adaette %>%
  dplyr::filter(PARAM == "Time to first occurrence of any adverse event") %>%
  dplyr::mutate(is_event = CNSR == 0) %>%
  dplyr::mutate(n_events = as.integer(is_event))

testthat::test_that("AET05 variant 1 is produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    estimate_incidence_rate(
      vars = "AVAL",
      n_events = "n_events",
      control = control_incidence_rate(num_pt_year = 100)
    )

  result <- build_table(lyt, anl, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET05 variant 2 is produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    estimate_incidence_rate(
      vars = "AVAL",
      n_events = "n_events",
      control = control_incidence_rate(conf_type = "exact", num_pt_year = 100)
    )

  result <- build_table(lyt, anl, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
