adsl <- adsl_raw
adlb <- adlb_raw

adsl <- df_explicit_na(adsl)
adlb <- df_explicit_na(adlb)

adsl_f <- adsl %>% filter(SAFFL == "Y")
adlb <- adlb %>% filter(SAFFL == "Y")

testthat::test_that("LBT14 variant 1: HIGH works as expected", {
  adlb_f <- adlb %>% filter(WGRHIFL == "Y")

  # Please note the step below can be skipped if you are using DTYPE PHANTOM
  adlb_out <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRHIFL" = "Y"))

  # Create new grouping variables ATOXGR_GP, BTOXGR_GP
  adlb_out <- adlb_out %>%
    mutate(
      ATOXGR_GP = factor(case_when(
        ATOXGR %in% c(0, -1, -2, -3, -4) ~ "Not High",
        ATOXGR == 1 ~ "1",
        ATOXGR == 2 ~ "2",
        ATOXGR == 3 ~ "3",
        ATOXGR == 4 ~ "4",
        ATOXGR == "<Missing>" ~ "Missing"
      ), levels = c("Not High", "1", "2", "3", "4", "Missing"))
    ) %>%
    mutate(
      BTOXGR_GP = factor(case_when(
        BTOXGR %in% c(0, -1, -2, -3, -4) ~ "Not High",
        BTOXGR == 1 ~ "1",
        BTOXGR == 2 ~ "2",
        BTOXGR == 3 ~ "3",
        BTOXGR == 4 ~ "4",
        BTOXGR == "<Missing>" ~ "Missing"
      ), levels = c("Not High", "1", "2", "3", "4", "Missing"))
    )

  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_rows_by(
      "PARAM",
      split_fun = drop_split_levels,
      label_pos = "topleft",
      split_label = "Parameter"
    ) %>%
    split_rows_by(
      "BTOXGR_GP",
      label_pos = "topleft",
      split_label = "    Baseline NCI-CTCAE Grade",
      indent_mod = 2L
    ) %>%
    summarize_num_patients(var = "USUBJID", .stats = c("unique_count"), unique_count_suffix = FALSE) %>%
    count_occurrences_by_grade("ATOXGR_GP", denom = "n", drop = FALSE, .indent_mods = 3L) %>%
    append_topleft("              Post-baseline NCI-CTCAE Grade") %>%
    build_table(df = adlb_out, alt_counts_df = adsl_f) %>%
    prune_table()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("LBT14 variant 2: LOW works as expected", {
  adlb_f <- adlb %>% filter(WGRLOFL == "Y")

  # Please note the step below can be skipped if you are using DTYPE PHANTOM
  adlb_out <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRLOFL" = "Y"))

  # Create new grouping variables ATOXGR_GP, BTOXGR_GP
  adlb_out <- adlb_out %>%
    mutate(
      ATOXGR_GP = factor(case_when(
        ATOXGR %in% c(0, 1, 2, 3, 4) ~ "Not Low",
        ATOXGR == -1 ~ "1",
        ATOXGR == -2 ~ "2",
        ATOXGR == -3 ~ "3",
        ATOXGR == -4 ~ "4",
        ATOXGR == "<Missing>" ~ "Missing"
      ), levels = c("Not Low", "1", "2", "3", "4", "Missing"))
    ) %>%
    mutate(
      BTOXGR_GP = factor(case_when(
        BTOXGR %in% c(0, 1, 2, 3, 4) ~ "Not Low",
        BTOXGR == -1 ~ "1",
        BTOXGR == -2 ~ "2",
        BTOXGR == -3 ~ "3",
        BTOXGR == -4 ~ "4",
        BTOXGR == "<Missing>" ~ "Missing"
      ), levels = c("Not Low", "1", "2", "3", "4", "Missing"))
    )

  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_rows_by(
      "PARAM",
      split_fun = drop_split_levels,
      label_pos = "topleft",
      split_label = "Parameter"
    ) %>%
    split_rows_by(
      "BTOXGR_GP",
      label_pos = "topleft",
      split_label = "    Baseline NCI-CTCAE Grade",
      indent_mod = 2L
    ) %>%
    summarize_num_patients(var = "USUBJID", .stats = c("unique_count"), unique_count_suffix = FALSE) %>%
    count_occurrences_by_grade("ATOXGR_GP", denom = "n", drop = FALSE, .indent_mods = 3L) %>%
    append_topleft("              Post-baseline NCI-CTCAE Grade") %>%
    build_table(df = adlb_out, alt_counts_df = adsl_f) %>%
    prune_table()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("LBT14 variant 3: LOW without baseline missing works as expected", {
  adlb_f <- adlb %>% filter(WGRHIFL == "Y")

  # Please note the step below can be skipped if you are using DTYPE PHANTOM
  adlb_out <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRHIFL" = "Y"))

  # Create new grouping variables ATOXGR_GP, BTOXGR_GP
  adlb_out <- adlb_out %>%
    filter(BTOXGR != "<Missing>") %>%
    mutate(
      ATOXGR_GP = factor(case_when(
        ATOXGR %in% c(0, -1, -2, -3, -4) ~ "Not High",
        ATOXGR == 1 ~ "1",
        ATOXGR == 2 ~ "2",
        ATOXGR == 3 ~ "3",
        ATOXGR == 4 ~ "4",
        ATOXGR == "<Missing>" ~ "Missing"
      ), levels = c("Not High", "1", "2", "3", "4", "Missing"))
    ) %>%
    mutate(
      BTOXGR_GP = factor(case_when(
        BTOXGR %in% c(0, -1, -2, -3, -4) ~ "Not High",
        BTOXGR == 1 ~ "1",
        BTOXGR == 2 ~ "2",
        BTOXGR == 3 ~ "3",
        BTOXGR == 4 ~ "4"
      ), levels = c("Not High", "1", "2", "3", "4"))
    )

  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_rows_by(
      "PARAM",
      split_fun = drop_split_levels,
      label_pos = "topleft",
      split_label = "Parameter"
    ) %>%
    split_rows_by(
      "BTOXGR_GP",
      label_pos = "topleft",
      split_label = "    Baseline NCI-CTCAE Grade",
      indent_mod = 2L
    ) %>%
    summarize_num_patients(var = "USUBJID", .stats = c("unique_count"), unique_count_suffix = FALSE) %>%
    count_occurrences_by_grade("ATOXGR_GP", denom = "n", drop = FALSE, .indent_mods = 3L) %>%
    append_topleft("              Post-baseline NCI-CTCAE Grade") %>%
    build_table(df = adlb_out, alt_counts_df = adsl_f) %>%
    prune_table()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("LBT14 variant 4: LOW and force 1 missing both baseline and post-baseline, then force the missing baseline as 0 as expected", { # nolint
  adlb_f <- adlb %>% filter(WGRLOFL == "Y")

  # Please note the step below can be skipped if you are using DTYPE PHANTOM
  adlb_out <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRLOFL" = "Y"))

  # Create new grouping variables ATOXGR_GP, BTOXGR_GP
  adlb_out <- adlb_out %>%
    mutate(
      ATOXGR_GP = factor(case_when(
        ATOXGR %in% c(0, 1, 2, 3, 4) ~ "Not Low",
        ATOXGR == -1 ~ "1",
        ATOXGR == -2 ~ "2",
        ATOXGR == -3 ~ "3",
        ATOXGR == -4 ~ "4",
        ATOXGR == "<Missing>" ~ "Missing"
      ), levels = c("Not Low", "1", "2", "3", "4"))
    ) %>%
    mutate(
      BTOXGR_GP = factor(case_when(
        BTOXGR %in% c(0, 1, 2, 3, 4, "<Missing>") ~ "Not Low",
        BTOXGR == -1 ~ "1",
        BTOXGR == -2 ~ "2",
        BTOXGR == -3 ~ "3",
        BTOXGR == -4 ~ "4"
      ), levels = c("Not Low", "1", "2", "3", "4", "Missing"))
    )

  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_rows_by(
      "PARAM",
      split_fun = drop_split_levels,
      label_pos = "topleft",
      split_label = "Parameter"
    ) %>%
    split_rows_by(
      "BTOXGR_GP",
      label_pos = "topleft",
      split_label = "    Baseline NCI-CTCAE Grade",
      indent_mod = 2L
    ) %>%
    summarize_num_patients(var = "USUBJID", .stats = c("unique_count"), unique_count_suffix = FALSE) %>%
    count_occurrences_by_grade("ATOXGR_GP", denom = "n", drop = FALSE, .indent_mods = 3L) %>%
    append_topleft("              Post-baseline NCI-CTCAE Grade") %>%
    build_table(df = adlb_out, alt_counts_df = adsl_f) %>%
    prune_table()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("LBT14 variant 5: HIGH with fillings works as expected", {
  adlb_f <- adlb %>% filter(WGRHIFL == "Y")

  # Please note the step below can be skipped if you are using DTYPE PHANTOM
  adlb_out <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRHIFL" = "Y"))

  # Create new grouping variables ATOXGR_GP, BTOXGR_GP
  adlb_out <- adlb_out %>%
    mutate(
      ATOXGR_GP = factor(case_when(
        ATOXGR %in% c(0, -1, -2, -3, -4) ~ "Not High",
        ATOXGR == 1 ~ "1",
        ATOXGR == 2 ~ "2",
        ATOXGR == 3 ~ "3",
        ATOXGR == 4 ~ "4",
        ATOXGR == "<Missing>" ~ "Missing"
      ), levels = c("Not High", "1", "2", "3", "4", "Missing"))
    ) %>%
    mutate(
      BTOXGR_GP = factor(case_when(
        BTOXGR %in% c(0, -1, -2, -3, -4) ~ "Not High",
        BTOXGR == 1 ~ "1",
        BTOXGR == 2 ~ "2",
        BTOXGR == 3 ~ "3",
        BTOXGR == 4 ~ "4",
        BTOXGR == "<Missing>" ~ "Missing"
      ), levels = c("Not High", "1", "2", "3", "4", "Missing"))
    )

  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_rows_by(
      "PARAM",
      split_fun = drop_split_levels,
      label_pos = "topleft",
      split_label = "Parameter"
    ) %>%
    split_rows_by(
      "BTOXGR_GP",
      label_pos = "topleft",
      split_label = "    Baseline NCI-CTCAE Grade",
      indent_mod = 2L
    ) %>%
    summarize_num_patients(var = "USUBJID", .stats = c("unique_count"), unique_count_suffix = FALSE) %>%
    count_occurrences_by_grade("ATOXGR_GP", denom = "n", drop = FALSE, .indent_mods = 3L) %>%
    append_topleft("              Post-baseline NCI-CTCAE Grade") %>%
    build_table(df = adlb_out, alt_counts_df = adsl_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
