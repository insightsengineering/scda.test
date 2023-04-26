adtte <- adtte_raw
saved_labels <- formatters::var_labels(adtte)
adtte_f <- adtte %>%
  dplyr::filter(
    PARAMCD == "OS",
    ARMCD %in% c("ARM A", "ARM B"),
    SEX %in% c("F", "M"),
    RACE %in% c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE")
  ) %>%
  dplyr::mutate(
    ARMCD = stats::relevel(droplevels(ARMCD), "ARM B"),
    SEX = droplevels(SEX),
    RACE = droplevels(RACE),
    EVENT = 1 - CNSR
  )
formatters::var_labels(adtte_f) <- c(saved_labels, "Event")

testthat::test_that("1. Cox Regression", {
  variables <- list(
    time = "AVAL", event = "EVENT", arm = "ARMCD",
    covariates = c("SEX", "RACE", "AGE")
  )

  result <- basic_table() %>%
    summarize_coxreg(
      variables = variables
    ) %>%
    build_table(df = adtte_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("2. Cox Regression (with Interaction Term)", {
  variables <- list(
    time = "AVAL", event = "EVENT", arm = "ARMCD",
    covariates = c("SEX", "RACE", "AGE")
  )
  control <- control_coxreg(interaction = TRUE)

  result <- basic_table() %>%
    summarize_coxreg(
      variables = variables,
      control = control,
      .stats = c("n", "hr", "ci", "pval", "pval_inter")
    ) %>%
    build_table(df = adtte_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("3. Cox Regression (specifying covariates)", {
  variables <- list(
    time = "AVAL", event = "EVENT", arm = "ARMCD",
    covariates = c("SEX", "RACE", "AGE")
  )
  control <- control_coxreg(interaction = TRUE)
  at <- list(AGE = c(30, 40, 50))

  result <- basic_table() %>%
    summarize_coxreg(
      variables = variables,
      control = control,
      at = at,
      .stats = c("n", "hr", "ci", "pval", "pval_inter")
    ) %>%
    build_table(df = adtte_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})


testthat::test_that("4. Cox Regression (setting strata, ties, and alpha level)", {
  variables <- list(
    time = "AVAL", event = "EVENT", arm = "ARMCD",
    covariates = c("SEX", "RACE", "AGE")
  )
  control <- control_coxreg(
    interaction = TRUE,
    conf_level = 0.90,
    ties = "efron"
  )
  at <- list(AGE = c(30, 40, 50))

  result <- basic_table() %>%
    summarize_coxreg(
      variables = variables,
      control = control,
      at = at,
      .stats = c("n", "hr", "ci", "pval", "pval_inter")
    ) %>%
    build_table(df = adtte_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
