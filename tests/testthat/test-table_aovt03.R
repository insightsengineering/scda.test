adsl <- adsl_raw
adqs <- adqs_raw

adqs_in <- adqs %>%
  filter(AVISIT %in% c("WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 5 DAY 36")) %>%
  droplevels() %>%
  filter(PARAM %in% c("BFI All Questions", "Fatigue Interference")) %>%
  mutate(CHG = ifelse(BMEASIFL == "Y", CHG, NA)) # only analyze evaluable population

testthat::test_that("AOVT03 is produced correctly", {
  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARMCD", ref_group = "ARM A", split_fun = ref_group_position("first")) %>%
    split_rows_by("PARAM", split_fun = drop_split_levels) %>%
    summarize_ancova(
      vars = "CHG",
      variables = list(arm = "ARMCD", covariates = c("BASE", "AVISIT", "AVISIT*ARMCD")),
      conf_level = 0.95,
      var_labels = "WEEK 1 DAY 8",
      table_names = "WEEK 1 DAY 8",
      interaction_y = "WEEK 1 DAY 8",
      interaction_item = "AVISIT"
    ) %>%
    summarize_ancova(
      vars = "CHG",
      variables = list(arm = "ARMCD", covariates = c("BASE", "AVISIT", "AVISIT*ARMCD")),
      conf_level = 0.95,
      var_labels = "WEEK 2 DAY 15",
      table_names = "WEEK 2 DAY 15",
      interaction_y = "WEEK 2 DAY 15",
      interaction_item = "AVISIT"
    ) %>%
    summarize_ancova(
      vars = "CHG",
      variables = list(arm = "ARMCD", covariates = c("BASE", "AVISIT", "AVISIT*ARMCD")),
      conf_level = 0.95,
      var_labels = "WEEK 5 DAY 36",
      table_names = "WEEK 5 DAY 36",
      interaction_y = "WEEK 5 DAY 36",
      interaction_item = "AVISIT"
    ) %>%
    build_table(adqs_in, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
