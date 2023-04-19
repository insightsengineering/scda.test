adtte <- adtte_raw
adtte_f <- subset(adtte, PARAMCD == "OS")
adtte_f <- within(
  data = subset(
    adtte_f,
    PARAMCD == "OS" &
      SEX %in% c("F", "M") &
      RACE %in% c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE")
  ),
  expr = {
    set.seed(1)
    ARMCD <- stats::relevel(ARMCD, "ARM B")
    SEX <- droplevels(SEX)
    RACE <- droplevels(RACE)
    X <- stats::rnorm(n = length(ARM))
  }
) %>%
  dplyr::mutate(EVENT = 1 - CNSR)

columns <- c("SEX", "AGE")
labels <- c("Sex", "Age")
for (i in seq_along(columns)) {
  attr(adtte_f[[columns[i]]], "label") <- labels[i]
}

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
