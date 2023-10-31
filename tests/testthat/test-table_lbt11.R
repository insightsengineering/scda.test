adsl <- adsl_raw
adsaftte <- adaette_raw

# Ensure character variables are converted to factors and empty strings and NAs are explicit missing levels.
adsl <- df_explicit_na(adsl)
adsaftte <- df_explicit_na(adsaftte)

adsl_safl <- dplyr::filter(adsl, .data$SAFFL == "Y")

anl <- adsaftte %>%
  dplyr::filter(
    SAFFL == "Y",
    PARAMCD == "HYSTTEUL"
  ) %>%
  dplyr::mutate(
    AVAL = .data$AVAL * dplyr::case_when( # convert to days, if possible
      .data$AVALU == "WEEKS" ~ 7,
      .data$AVALU == "MONTHS" ~ 30.4375,
      .data$AVALU == "YEARS" ~ 365,
      TRUE ~ 1
    ),
    AVALU = factor(dplyr::case_when(
      .data$AVALU %in% c("WEEKS", "MONTHS", "YEARS") ~ factor("DAYS"),
      TRUE ~ .data$AVALU
    ), levels = "DAYS"),
    is_event = CNSR == 0, # this will be a LLT event
    event_grp = factor(
      dplyr::case_when(
        CNSR == 0 ~ "Patients with LLT event (%)",
        CNSR == 1 ~ "Patients without LLT event (%)"
      ),
      levels = c(
        "Patients with LLT event (%)",
        "Patients without LLT event (%)"
      )
    )
  )

testthat::test_that("LBT11 variant 1 works as expected", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ARMCD", ref_group = "ARM A", split_fun = ref_group_position("first")) %>%
    count_occurrences(vars = "event_grp") %>%
    surv_time(
      vars = "AVAL",
      var_labels = paste0("Time to 1st LLT Event (", levels(anl$AVALU), ")"),
      is_event = "is_event",
      table_names = "time_to_event"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = "Unstratified Analysis",
      control = control_coxph(pval_method = "log-rank"),
      table_names = "coxph_unstratified"
    )

  result <- build_table(lyt, df = anl, alt_counts_df = adsl_safl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("LBT11 variant 2 works as expected", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ARMCD", ref_group = "ARM A", split_fun = ref_group_position("first")) %>%
    count_occurrences(vars = "event_grp") %>%
    surv_time(
      vars = "AVAL",
      var_labels = paste0("Time to 1st LLT Event (", levels(anl$AVALU), ")"),
      is_event = "is_event",
      table_names = "time_to_event"
    )

  result <- build_table(lyt, df = anl, alt_counts_df = adsl_safl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("LBT11 variant 3 works as expected", {
  strata <- c("RACE", "SEX")

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ARMCD", ref_group = "ARM A", split_fun = ref_group_position("first")) %>%
    count_occurrences(vars = "event_grp") %>%
    surv_time(
      vars = "AVAL",
      var_labels = paste0("Time to 1st LLT Event (", levels(anl$AVALU), ")"),
      is_event = "is_event",
      table_names = "time_to_event"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = "Unstratified Analysis",
      control = control_coxph(pval_method = "log-rank"),
      table_names = "coxph_unstratified"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = paste0("Stratified By: ", paste(strata, collapse = ", ")),
      strat = strata,
      table_names = "coxph_stratified"
    )

  result <- build_table(lyt, df = anl, alt_counts_df = adsl_safl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
