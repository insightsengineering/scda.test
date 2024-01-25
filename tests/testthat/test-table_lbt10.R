adsl <- adsl_pharmaverse
adhy <- full_join(
  (pharmaverseadam::adlb %>%
    filter(PARAMCD %in% c("BILI") & !(DTYPE %in% c("MINIMUM", "MAXIMUM"))) %>%
    select(USUBJID, ACTARMCD, AVISIT, PARAMCD, AVAL, ANRHI) %>%
    rename(
      BILI = PARAMCD,
      BILIVAL = AVAL,
      BILIRHI = ANRHI
    )),
  (pharmaverseadam::adlb %>%
    filter(PARAMCD %in% c("ALT", "AST") & !(DTYPE %in% c("MINIMUM", "MAXIMUM"))) %>%
    select(USUBJID, ACTARMCD, AVISIT, PARAMCD, AVAL, ANRHI) %>%
    rename(
      ALTAST = PARAMCD,
      ALTASTVAL = AVAL,
      ALTASTRHI = ANRHI
    )),
  by = join_by(USUBJID, ACTARMCD, AVISIT)
) %>%
  unique() %>%
  mutate(
    TBILI_CAT = factor(
      case_when(
        BILIVAL <= 1 * BILIRHI ~ "Total Bilirubin <= 2xULN",
        BILIVAL > 1 * BILIRHI ~ "Total Bilirubin > 2xULN",
        TRUE ~ "Criteria not met"
      ),
      levels = c(
        "Total Bilirubin <= 2xULN",
        "Total Bilirubin > 2xULN",
        "Criteria not met"
      )
    ),
    ALTAST_CAT = factor(
      case_when(
        ALTAST %in% c("ALT") & (ALTASTVAL > 3 * ALTASTRHI) ~ "ALT >3 - <= 5xULN",
        ALTAST %in% c("ALT") & (ALTASTVAL > 5 * ALTASTRHI) ~ "ALT >5 - <= 10xULN",
        ALTAST %in% c("ALT") & (ALTASTVAL > 10 * ALTASTRHI) ~ "ALT >10 - <= 20xULN",
        ALTAST %in% c("ALT") & (ALTASTVAL > 20 * ALTASTRHI) ~ "ALT >20xULN",
        ALTAST %in% c("AST") & (ALTASTVAL > 3 * ALTASTRHI) ~ "AST >3 - <= 5xULN",
        ALTAST %in% c("AST") & (ALTASTVAL > 5 * ALTASTRHI) ~ "ALT >5 - <= 10xULN",
        ALTAST %in% c("AST") & (ALTASTVAL > 10 * ALTASTRHI) ~ "AST >10 - <= 20xULN",
        ALTAST %in% c("AST") & (ALTASTVAL > 20 * ALTASTRHI) ~ "AST >20xULN",
        TRUE ~ "Criteria not met"
      ),
      levels = c(
        "ALT >3 - <= 5xULN", "ALT >5 - <= 10xULN", "ALT >10 - <= 20xULN", "ALT > 20xULN",
        "AST >3 - <= 5xULN", "AST >5 - <= 10xULN", "AST >10 - <= 20xULN", "AST > 20xULN",
        "Criteria not met"
      )
    )
  )

anl <- adhy %>%
  group_by(USUBJID, ACTARMCD, TBILI_CAT, .drop = FALSE) %>%
  count(ALTAST_CAT) %>%
  ungroup() %>%
  mutate(AVALC_FORMAT = ifelse(n > 0, as.character(ALTAST_CAT), "Criteria not met")) %>%
  df_explicit_na()

# Ensure character variables are converted to factors and empty strings and NAs are explicit missing levels.
adsl <- df_explicit_na(adsl)

testthat::test_that("LBT10 variant 1 works as expected", {
  tbl <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARMCD") %>%
    split_rows_by("TBILI_CAT") %>%
    split_rows_by("ALTAST_CAT", split_fun = drop_split_levels, child_labels = "hidden") %>%
    count_occurrences(
      vars = "AVALC_FORMAT",
      .stats = c("fraction"),
      denom = "n",
      drop = TRUE
    ) %>%
    build_table(anl, alt_counts_df = adsl)

  criteria_fun <- function(tr) {
    row_label <- obj_label(tr)
    ifelse(row_label == "Criteria not met", TRUE, FALSE)
  }

  result <- tbl %>% trim_rows(criteria = criteria_fun)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("LBT10 variant 2 works as expected", {
  tbl2 <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARMCD") %>%
    split_rows_by("TBILI_CAT", split_fun = remove_split_levels("Total Bilirubin > 2xULN")) %>%
    split_rows_by("ALTAST_CAT", split_fun = drop_split_levels, child_labels = "hidden") %>%
    count_occurrences(
      vars = "AVALC_FORMAT",
      .stats = c("fraction"),
      denom = "n",
      drop = TRUE
    ) %>%
    build_table(anl, alt_counts_df = adsl)

  criteria_fun <- function(tr) {
    row_label <- obj_label(tr)
    ifelse(row_label == "Criteria not met", TRUE, FALSE)
  }

  result <- tbl2 %>% trim_rows(criteria = criteria_fun)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
