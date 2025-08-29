# extra special case situation to scenario from test-table_dmt01.R
# Tests DMT01 with corner case situation to demonstrate bug in analyze is resolved with empty factor data
# tern@1431-bug-s_summaryfactor
set.seed(1)

adsl <- pharmaverseadam::adsl
adsl <- adsl %>%
  mutate(BMRKR1 = rnorm(nrow(adsl), 3.5, 10)) %>%
  mutate(STRATA1 = factor(sample(c("A", "B", "C"), nrow(adsl), TRUE)))

adsl <- df_explicit_na(adsl)

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
    BMRKR1_CAT = NA_character_
  ) %>%
  var_relabel(
    BMRKR1_CAT = "Biomarker 1 Categories"
  )

adsl <- df_explicit_na(adsl, na_level = "<Missing>")


testthat::test_that("DMT01 variant factor with only Missing values", {
  vars <- c("AGE", "AGEGR1", "BMRKR1_CAT")
  var_labels <- c(
    "Age (yr)",
    "Age Group",
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
