adsl <- adsl_raw
adae <- adae_raw

adsl <- df_explicit_na(adsl)
adae <- df_explicit_na(
  adae,
  omit_columns = c("SMQ01NAM", "SMQ01SC", "SMQ02NAM", "SMQ02SC", "CQ01NAM", "STUDYID", "USUBJID")
)

df_max <- aggregate(as.numeric(AETOXGR) ~ USUBJID, data = adae, FUN = max, drop = FALSE)
colnames(df_max) <- c("USUBJID", "WTOXGR")

adae <- adae %>%
  left_join(df_max, by = c("USUBJID")) %>%
  mutate(
    WTOXGR = factor(WTOXGR, levels = c("1", "2", "3", "4", "5")),
    fl_ser = AESER == "Y"
  ) %>%
  mutate(
    WTOXGR = forcats::fct_recode(
      WTOXGR,
      "Grade 1" = "1",
      "Grade 2" = "2",
      "Grade 3" = "3",
      "Grade 4" = "4",
      "Grade 5" = "5"
    ),
    AEOUT = forcats::fct_recode(
      AEOUT,
      "Fatal outcome" = "FATAL",
      "Unresolved" = "NOT RECOVERED/NOT RESOLVED",
      "Recovered/Resolved" = "RECOVERED/RESOLVED",
      "Resolved with sequelae" = "RECOVERED/RESOLVED WITH SEQUELAE",
      "Recovering/Resolving" = "RECOVERING/RESOLVING",
      "Unknown outcome" = "UNKNOWN"
    )
  ) %>%
  var_relabel(
    WTOXGR = "Worst overall grade",
    fl_ser = "Number of patients with at least one serious AE"
  )

# Add AE flag to adsl.
adsl1 <- adsl %>%
  mutate(AEFL = ifelse(USUBJID %in% adae$USUBJID, TRUE, FALSE)) %>%
  var_relabel(AEFL = "At least one AE")

testthat::test_that("RMPT06 variant 1 is produced correctly", {
  lyt_adsl <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    estimate_proportion(
      vars = "AEFL",
      method = "clopper-pearson",
      .labels = c(
        n_prop = "Number of patients with at least one adverse event",
        prop_ci = "95% CI for % of patients with at least one AE (Clopper-Pearson)"
      ),
      table_names = "est_prop"
    )

  result_adsl <- build_table(lyt_adsl, df = adsl1, alt_counts_df = adsl1)

  # Layout for variables from adae dataset.
  lyt_adae <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    count_values(
      "STUDYID",
      values = as.character(unique(adae$STUDYID)),
      .stats = "count",
      .labels = c(count = "Total number of AEs"),
      table_names = "total_aes"
    ) %>%
    count_occurrences(
      "WTOXGR",
      var_labels = "Number of patients with at least one AE by worst grade",
      show_labels = "visible"
    ) %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = "fl_ser"
    ) %>%
    count_occurrences(
      "AEOUT",
      denom = "n",
      var_labels = "Number of patients with at least one AE by outcome",
      show_labels = "visible"
    )

  result_adae <- build_table(lyt_adae, df = adae, alt_counts_df = adsl)

  # Combine tables.
  col_info(result_adsl) <- col_info(result_adae)
  result <- rbind(
    result_adsl,
    result_adae[seq_len(nrow(result_adae)), ]
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("RMPT06 variant 2 is produced correctly", {
  lyt_adsl <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    estimate_proportion(
      vars = "AEFL",
      method = "clopper-pearson",
      .labels = c(
        n_prop = "Number of patients with at least one adverse event",
        prop_ci = "95% CI for % of patients with at least one AE (Clopper-Pearson)"
      ),
      table_names = "est_prop"
    )

  result_adsl <- build_table(lyt_adsl, df = adsl1, alt_counts_df = adsl1)

  # Layout for variables from adae dataset.
  lyt_adae <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    count_values(
      "STUDYID",
      values = as.character(unique(adae$STUDYID)),
      .stats = "count",
      .labels = c(count = "Total number of AEs"),
      table_names = "total_aes"
    ) %>%
    count_occurrences(
      "WTOXGR",
      var_labels = "Number of patients with at least one AE by worst grade",
      show_labels = "visible"
    ) %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = "fl_ser",
      denom = "N_col"
    )

  result_adae <- build_table(lyt_adae, df = adae, alt_counts_df = adsl)

  # Combine tables.
  col_info(result_adsl) <- col_info(result_adae)
  result <- rbind(
    result_adsl,
    result_adae[seq_len(nrow(result_adae)), ]
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("RMPT06 variant 3 is produced correctly", {
  lyt_adsl <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM", ref_group = "A: Drug X", split_fun = ref_group_position("first")) %>%
    estimate_proportion(
      vars = "AEFL",
      method = "clopper-pearson",
      .labels = c(
        n_prop = "Number of patients with at least one adverse event",
        prop_ci = "95% CI for % of patients with at least one AE"
      ),
      table_names = "est_prop"
    ) %>%
    estimate_proportion_diff(
      vars = "AEFL",
      .labels = c(
        diff = "Difference in % of patients with at least one AE",
        diff_ci = "95% CI of difference (Wald, with correction)"
      ),
      table_names = "est_prop_diff"
    )

  result_adsl <- build_table(lyt_adsl, df = adsl1, alt_counts_df = adsl1)

  # Layout for variables from adae dataset.
  lyt_adae <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    count_values(
      "STUDYID",
      values = as.character(unique(adae$STUDYID)),
      .stats = "count",
      .labels = c(count = "Total number of AEs"),
      table_names = "total_aes"
    ) %>%
    count_occurrences(
      "WTOXGR",
      var_labels = "Number of patients with at least one AE by worst grade",
      show_labels = "visible"
    ) %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = "fl_ser",
      denom = "N_col"
    ) %>%
    count_occurrences(
      "AEOUT",
      denom = "n",
      var_labels = "Number of patients with at least one AE by outcome",
      show_labels = "visible"
    )

  result_adae <- build_table(lyt_adae, df = adae, alt_counts_df = adsl)

  # Combine tables.
  col_info(result_adsl) <- col_info(result_adae)
  result <- rbind(
    result_adsl,
    result_adae[seq_len(nrow(result_adae)), ]
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("RMPT06 variant 4 is produced correctly", {
  lyt_adsl <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM", ref_group = "A: Drug X", split_fun = ref_group_position("first")) %>%
    estimate_proportion(
      vars = "AEFL",
      conf_level = 0.90,
      method = "clopper-pearson",
      .labels = c(
        n_prop = "Number of patients with at least one adverse event",
        prop_ci = "90% CI for % of patients with at least one AE (Clopper-Pearson)"
      ),
      table_names = "est_prop"
    ) %>%
    estimate_proportion_diff(
      vars = "AEFL",
      conf_level = 0.90,
      method = "newcombe",
      .labels = c(
        diff = "Difference in % of patients with at least one AE",
        diff_ci = "90% CI of difference (Newcombe)"
      ),
      table_names = "est_prop_diff"
    )

  result_adsl <- build_table(lyt_adsl, df = adsl1, alt_counts_df = adsl1)

  # Layout for variables from adae dataset.
  lyt_adae <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    count_values(
      "STUDYID",
      values = as.character(unique(adae$STUDYID)),
      .stats = "count",
      .labels = c(count = "Total number of AEs"),
      table_names = "total_aes"
    ) %>%
    count_occurrences(
      "WTOXGR",
      var_labels = "Number of patients with at least one AE by worst grade",
      show_labels = "visible"
    ) %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = "fl_ser",
      denom = "N_col"
    ) %>%
    count_occurrences(
      "AEOUT",
      denom = "n",
      var_labels = "Number of patients with at least one AE by outcome",
      show_labels = "visible"
    )

  result_adae <- build_table(lyt_adae, df = adae, alt_counts_df = adsl)

  # Combine tables.
  col_info(result_adsl) <- col_info(result_adae)
  result <- rbind(
    result_adsl,
    result_adae[seq_len(nrow(result_adae)), ]
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
