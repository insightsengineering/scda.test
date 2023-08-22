# Tests DMT01

adsl <- adsl_raw
advs <- advs_raw
adsub <- adsub_raw

adsl <- df_explicit_na(adsl)
advs <- df_explicit_na(advs)
adsub <- df_explicit_na(adsub)

adsl <- adsl %>%
  mutate(
    SEX = factor(case_when(
      SEX == "M" ~ "Male",
      SEX == "F" ~ "Female",
      SEX == "U" ~ "Unknown",
      SEX == "UNDIFFERENTIATED" ~ "Undifferentiated"
    )),
    AGEGR1 = factor(
      case_when(
        between(AGE, 18, 40) ~ "18-40",
        between(AGE, 41, 64) ~ "41-64",
        AGE > 64 ~ ">=65"
      ),
      levels = c("18-40", "41-64", ">=65")
    ),
    BMRKR1_CAT = factor(
      case_when(
        BMRKR1 < 3.5 ~ "LOW",
        BMRKR1 >= 3.5 & BMRKR1 < 10 ~ "MEDIUM",
        BMRKR1 >= 10 ~ "HIGH"
      ),
      levels = c("LOW", "MEDIUM", "HIGH")
    )
  ) %>%
  var_relabel(
    BMRKR1_CAT = "Biomarker 1 Categories"
  )

get_param_advs <- function(pname, plabel) {
  ds <- advs %>%
    filter(PARAM == plabel & AVISIT == "BASELINE") %>%
    select(USUBJID, AVAL)

  colnames(ds) <- c("USUBJID", pname)

  ds
}

get_param_adsub <- function(pname, plabel) {
  ds <- adsub %>%
    filter(PARAM == plabel) %>%
    select(USUBJID, AVAL)

  colnames(ds) <- c("USUBJID", pname)

  ds
}
adsl <- adsl %>%
  left_join(get_param_advs("SBP", "Systolic Blood Pressure"), by = "USUBJID") %>%
  left_join(get_param_advs("DBP", "Diastolic Blood Pressure"), by = "USUBJID") %>%
  left_join(get_param_advs("WGT", "Weight"), by = "USUBJID") %>%
  left_join(get_param_adsub("BBMISI", "Baseline BMI"), by = "USUBJID")

testthat::test_that("DMT01 variant 1 is produced correctly", {
  vars <- c("AGE", "AGEGR1", "SEX", "ETHNIC", "RACE", "BMRKR1")
  var_labels <- c(
    "Age (yr)",
    "Age Group",
    "Sex",
    "Ethnicity",
    "Race",
    "Continous Level Biomarker 1"
  )

  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ACTARM") %>%
    add_overall_col("All Patients") %>%
    analyze_vars(
      vars = vars,
      var_labels = var_labels
    ) %>%
    build_table(adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("DMT01 variant 2 is produced correctly", {
  vars <- c("AGE", "AGEGR1", "SEX", "ETHNIC", "RACE", "BMRKR1_CAT")
  var_labels <- c(
    "Age (yr)",
    "Age Group",
    "Sex",
    "Ethnicity",
    "Race",
    "Biomarker 1 Categories"
  )

  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ACTARM") %>%
    analyze_vars(
      vars = vars,
      var_labels = var_labels
    ) %>%
    build_table(adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("DMT01 variant 3 is produced correctly", {
  split_fun <- drop_split_levels

  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ACTARM") %>%
    analyze_vars(
      vars = c("AGE", "SEX", "RACE"),
      var_labels = c("Age", "Sex", "Race")
    ) %>%
    split_rows_by("STRATA1",
      split_fun = split_fun
    ) %>%
    analyze_vars("BMRKR1") %>%
    build_table(adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("DMT01 variant 4 is produced correctly", {
  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ACTARM") %>%
    analyze_vars(
      vars = c("AGE", "SEX", "RACE", "DBP", "SBP"),
      var_labels = c(
        "Age (yr)",
        "Sex",
        "Race",
        "Diastolic Blood Pressure",
        "Systolic Blood Pressure"
      )
    ) %>%
    build_table(adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("DMT01 variant 5 is produced correctly", {
  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ACTARM") %>%
    analyze_vars(
      vars = c("AGE", "SEX", "RACE", "BBMISI"),
      var_labels = c(
        "Age (yr)",
        "Sex",
        "Race",
        "Baseline BMI"
      )
    ) %>%
    build_table(adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
