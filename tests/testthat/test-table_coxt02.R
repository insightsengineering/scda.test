adtte <- adtte_raw
adtte_f <- adtte %>%
  dplyr::filter(
    PARAMCD == "OS",
    SEX %in% c("F", "M"),
    RACE %in% c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE")
  ) %>%
  dplyr::mutate(
    ARMCD = stats::relevel(ARMCD, "ARM B"),
    SEX = droplevels(SEX),
    RACE = droplevels(RACE),
    EVENT = 1 - CNSR
  ) %>%
  formatters::var_relabel(
    SEX = "Sex",
    AGE = "Age"
  )

testthat::test_that("COXT02 default variant 1 is produced correctly", {
  variables <- list(
    time = "AVAL", event = "EVENT", arm = "ARMCD",
    covariates = c("SEX", "AGE")
  )

  result <- basic_table() %>%
    summarize_coxreg(variables = variables, multivar = TRUE) %>%
    build_table(adtte_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("COXT02 variant 5 is produced correctly", {
  variables <- list(
    time = "AVAL", event = "EVENT", arm = "ARMCD",
    covariates = c("SEX", "AGE"), strata = "RACE"
  )
  control <- control_coxreg(
    conf_level = 0.9,
    ties = "efron"
  )

  result <- basic_table() %>%
    summarize_coxreg(variables = variables, control = control, multivar = TRUE, .stats = c("hr", "ci")) %>%
    build_table(adtte_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
