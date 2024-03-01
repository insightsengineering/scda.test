adsl <- adsl_raw
adqs <- adqs_raw

# Ensure character variables are converted to factors and empty strings and NAs are explicit missing levels.
adsl <- df_explicit_na(adsl)
adqs <- df_explicit_na(adqs)

testthat::test_that("CMHT01 variant 1 is produced correctly", {
  anl_01 <- adqs %>%
    filter(PARAMCD == "FKSI-FWB" & AVISIT == "WEEK 1 DAY 8") %>%
    mutate(is_rsp = PCHG > 15) %>%
    mutate(ARM = relevel(ARM, ref = "A: Drug X")) %>%
    var_relabel(ARM = "Description of Planned Arm") %>%
    mutate(strata = interaction(STRATA1, STRATA2, drop = TRUE))

  lyt_01 <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ARM", ref_group = "A: Drug X", split_fun = ref_group_position("first")) %>%
    estimate_proportion(vars = "is_rsp", table_names = "est_prop") %>%
    estimate_proportion_diff(
      var_labels = "Unstratified Analysis",
      vars = "is_rsp",
      show_labels = "visible",
      table_names = "est_prop_diff"
    ) %>%
    test_proportion_diff(vars = "is_rsp", table_names = "test_prop") %>%
    estimate_odds_ratio(vars = "is_rsp", table_names = "est_or") %>%
    estimate_proportion_diff(
      var_labels = "Stratified Analysis",
      vars = "is_rsp",
      show_labels = "visible",
      method = "cmh",
      variables = list(strata = "strata"),
      table_names = "est_prop_diff_strat"
    ) %>%
    test_proportion_diff(
      vars = "is_rsp",
      method = "cmh",
      variables = list(strata = "strata"),
      table_names = "test_prop_strat"
    ) %>%
    estimate_odds_ratio(
      vars = "is_rsp",
      variables = list(strata = "strata", arm = "ARM"),
      table_names = "est_or_strat"
    )

  # https://github.com/therneau/survival/issues/240
  withr::with_options(
    opts_partial_match_old,
    result <- build_table(
      lyt = lyt_01,
      df = anl_01,
      alt_counts_df = adsl
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("CMHT01 variant 2 is produced correctly", {
  anl_02 <- adqs %>%
    filter(AVISIT == "WEEK 1 DAY 8") %>%
    mutate(is_rsp = PCHG > 15) %>%
    mutate(PARAM = droplevels(PARAM)) %>%
    mutate(ARM = relevel(ARM, ref = "A: Drug X")) %>%
    var_relabel(ARM = "Description of Planned Arm") %>%
    mutate(strata = interaction(STRATA1, STRATA2, drop = TRUE))

  var_labels(anl_02)["PARAM"] <- "Parameter"

  split_fun <- drop_split_levels

  lyt_02 <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ARM", ref_group = "A: Drug X", split_fun = ref_group_position("first")) %>%
    split_rows_by(
      var = "PARAM",
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(anl_02$PARAM)
    ) %>%
    estimate_proportion(vars = "is_rsp", table_names = "est_prop") %>%
    estimate_proportion_diff(
      var_labels = "Unstratified Analysis",
      vars = "is_rsp",
      show_labels = "visible",
      table_names = "est_prop_diff"
    ) %>%
    test_proportion_diff(vars = "is_rsp", table_names = "test_prop") %>%
    estimate_odds_ratio(vars = "is_rsp", table_names = "est_or") %>%
    estimate_proportion_diff(
      var_labels = "Stratified Analysis",
      vars = "is_rsp",
      show_labels = "visible",
      method = "cmh",
      variables = list(strata = "strata"),
      table_names = "est_prop_diff_strat"
    ) %>%
    test_proportion_diff(
      vars = "is_rsp",
      method = "cmh",
      variables = list(strata = "strata"),
      table_names = "test_prop_strat"
    ) %>%
    estimate_odds_ratio(
      vars = "is_rsp",
      variables = list(strata = "strata", arm = "ARM"),
      table_names = "est_or_strat"
    )

  # https://github.com/therneau/survival/issues/240
  withr::with_options(
    opts_partial_match_old,
    result <- build_table(
      lyt = lyt_02,
      df = anl_02,
      alt_counts_df = adsl
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
