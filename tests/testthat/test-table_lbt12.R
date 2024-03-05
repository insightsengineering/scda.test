adhy <- pharmaverseadam::adlb %>%
  filter(PARAMCD %in% c("ALT", "AST") & !(DTYPE %in% c("MINIMUM", "MAXIMUM")) & AVISIT != "Baseline" & SAFFL == "Y") %>%
  mutate(
    APERIODC = ifelse(
      AVISIT %in% c("Unscheduled 1.1", "Unscheduled 1.2", "Unscheduled 1.3", "Week 2"),
      "PERIOD 1",
      "PERIOD 2"
    ),
    AVAL2 = ifelse(AVAL > 3 * ANRHI, "Y", "N")
  ) %>%
  select(USUBJID, ACTARM, AVISIT, APERIODC, PARAMCD, AVAL2)

adhy_altast <- adhy %>%
  pivot_wider(., names_from = PARAMCD, values_from = AVAL2) %>%
  mutate(
    PARAMCD = "ALTAST",
    AVAL2 = ifelse(ALT == "Y" | AST == "Y", "Y", "N")
  ) %>%
  select(USUBJID, ACTARM, AVISIT, APERIODC, PARAMCD, AVAL2)

anl <- bind_rows(adhy, adhy_altast)
anl$APERIODC <- as.factor(anl$APERIODC) # to ensure the table is built even if there is no patients after filtering
anl$ACTARM <- as.factor(anl$ACTARM) # to ensure the table is built even if there is no patients after filtering

anl <- anl %>%
  mutate(
    ARM_AVALC = factor(
      case_when(
        AVAL2 == "Y" ~ as.character(ACTARM),
        TRUE ~ "Criteria not met"
      ),
      levels = c(levels(anl$ACTARM), "Criteria not met")
    ),
    PARAM = factor(
      case_when(
        PARAMCD == "ALT" ~ "AST >3x ULN",
        PARAMCD == "AST" ~ "ALT >3x ULN",
        PARAMCD == "ALTAST" ~ "AST >3x ULN or ALT >3x ULN"
      ),
      levels = c("AST >3x ULN", "ALT >3x ULN", "AST >3x ULN or ALT >3x ULN")
    ),
    TITLE = factor("First Elevated Result Occurring During")
  )

anl <- df_explicit_na(anl)

testthat::test_that("LBT12 works as expected", {
  result <- basic_table() %>%
    split_cols_by("TITLE") %>%
    split_cols_by("APERIODC") %>%
    split_rows_by("PARAM") %>%
    split_rows_by("ACTARM", split_fun = drop_split_levels, child_labels = "hidden") %>%
    count_occurrences("ARM_AVALC", .stats = "fraction", denom = "n", drop = TRUE) %>%
    build_table(anl)

  criteria_fun <- function(tr) {
    row_label <- obj_label(tr)
    dplyr::if_else(row_label == "Criteria not met", TRUE, FALSE)
  }

  result <- result %>% trim_rows(criteria = criteria_fun)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
