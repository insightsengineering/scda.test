adsl <- adsl_raw
adhy <- adhy_raw

adhy_liver <- adhy %>%
  filter(PARAMCD %in% c(
    "BL2AL2CB", "BL2AS2CB", "BG2AL2CB", "BG2AS2CB", "B2A2L2CB", "B2A2S2CB", "B2A5L2CB", "B2A5S2CB"
  )) %>%
  mutate(PARAMCAT = case_when(
    PARAMCD %in% c("BL2AL2CB", "BL2AS2CB") ~ "Total Bilirubin <= 2xULN",
    PARAMCD %in% c("BG2AL2CB", "BG2AS2CB") ~ "Total Bilirubin > 2xULN",
    PARAMCD %in% c("B2A2L2CB", "B2A2S2CB") ~ "Total Bilirubin > 2xULN and Alkaline Phosphatase <= 2xULN",
    PARAMCD %in% c("B2A5L2CB", "B2A5S2CB") ~ "Total Bilirubin > 2xULN and Alkaline Phosphatase <= 5xULN"
  )) %>%
  mutate(AVALC_FORMAT = case_when(
    PARAMCD %in% c("BL2AL2CB", "BG2AL2CB", "B2A2L2CB", "B2A5L2CB") & AVALC == "Y" ~ "ALT >3xULN at 2 Visits",
    PARAMCD %in% c("BL2AS2CB", "BG2AS2CB", "B2A2S2CB", "B2A5S2CB") & AVALC == "Y" ~ "AST >3xULN at 2 Visits",
    TRUE ~ "Criteria not met"
  ))

# Ensure character variables are converted to factors and empty strings and NAs are explicit missing levels.
adsl <- df_explicit_na(adsl)
anl <- df_explicit_na(adhy_liver)

testthat::test_that("LBT10_BL variant 1 works as expected", {
  tbl <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARMCD") %>%
    split_rows_by("PARAMCAT") %>%
    split_rows_by("PARAM", split_fun = drop_split_levels, child_labels = "hidden") %>%
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

testthat::test_that("LBT10_BL variant 2 works as expected", {
  tbl2 <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARMCD") %>%
    split_rows_by("PARAMCAT", split_fun = remove_split_levels("Total Bilirubin > 2xULN")) %>%
    split_rows_by("PARAM", split_fun = drop_split_levels, child_labels = "hidden") %>%
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
