# Test variants for AET05.
adsl <- adsl_pharmaverse %>%
  filter(SAFFL == "Y")
anl <- adlb_pharmaverse %>%
  filter(PARAMCD %in% c("AST", "ALT", "BILI") & is.na(DTYPE)) %>%
  mutate(
    CRIT1 = case_when(
      PARAMCD == "AST" ~ "AST >=3xULN",
      PARAMCD == "ALT" ~ "ALT >=3xULN",
      PARAMCD == "BILI" ~ "BILI >=2xULN"
    ),
    CRIT1FL = case_when(
      (AVAL / ANRHI >= 3) & PARAMCD %in% c("AST", "ALT") ~ "Y",
      (AVAL / ANRHI >= 2 & PARAMCD == "BILI") ~ "Y"
    )
  ) %>%
  select(STUDYID, USUBJID, ARMCD, ADT, CRIT1, CRIT1FL) %>%
  pivot_wider(names_from = CRIT1, values_from = CRIT1FL) %>%
  mutate(
    HYLAW = ifelse((`AST >=3xULN` == "Y" | `ALT >=3xULN` == "Y") & `BILI >=2xULN` == "Y", "Y", NA_character_)
  ) %>%
  select(USUBJID, HYLAW, ADT) %>%
  group_by(USUBJID, HYLAW) %>%
  mutate(
    LASTKNOWN = max(ADT),
    HYDT = min(ADT)
  ) %>%
  ungroup() %>%
  mutate(ADT2 = as.Date(ifelse(is.na(HYLAW), LASTKNOWN, HYDT))) %>%
  select(USUBJID, HYLAW, ADT2) %>%
  unique() %>%
  left_join(
    select(adsl, USUBJID, ARMCD, TRTSDT, SEX, RACE),
    .,
    by = "USUBJID"
  ) %>%
  mutate(
    PARAM = "Time to Hy's Law Elevation in relation to ULN",
    AVAL = as.numeric(difftime(ADT2, TRTSDT, units = "days")),
    AVAL = ifelse(is.na(AVAL) & !is.na(ARMCD), 1, AVAL),
    AVALU = ifelse(!is.na(AVAL), "DAYS", NA_character_),
    AVALU = as.factor(AVALU),
    CNSR = ifelse(is.na(HYLAW), 1, 0) # original pharmaverseadam data
  )

anl$CNSR[1:80] <- 0

anl <- anl %>%
  mutate(
    is_event = CNSR == 0,
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
  ) %>%
  filter(AVAL > 0)

testthat::test_that("LBT11 variant 1 works as expected", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ARMCD", ref_group = "Pbo", split_fun = ref_group_position("first")) %>%
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

  result <- build_table(lyt, df = anl, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("LBT11 variant 2 works as expected", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ARMCD", ref_group = "Pbo", split_fun = ref_group_position("first")) %>%
    count_occurrences(vars = "event_grp") %>%
    surv_time(
      vars = "AVAL",
      var_labels = paste0("Time to 1st LLT Event (", levels(anl$AVALU), ")"),
      is_event = "is_event",
      table_names = "time_to_event"
    )

  result <- build_table(lyt, df = anl, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("LBT11 variant 3 works as expected", {
  strata <- c("RACE", "SEX")

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ARMCD", ref_group = "Pbo", split_fun = ref_group_position("first")) %>%
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
      strata = strata,
      table_names = "coxph_stratified"
    )

  result <- build_table(lyt, df = anl, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
