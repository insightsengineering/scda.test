adsl <- adsl_pharmaverse
adlb <- adlb_pharmaverse %>%
  mutate(
    WGRLOVFL = ifelse(AVISIT == "POST-BASELINE MINIMUM", "Y", ""),
    WGRHIVFL = ifelse(AVISIT == "POST-BASELINE MAXIMUM", "Y", "")
  )

adsl <- df_explicit_na(adsl)
adlb <- df_explicit_na(adlb)

# Please note that in real clinical data, population flag like SAFFL, and parameter category like PARCAT2 needs to be
# selected properly.
adsl_f <- adsl %>% filter(SAFFL == "Y")
adlb <- adlb %>% filter(PARAMCD == "ALB" & ANL01FL == "Y")

testthat::test_that("LBT13 variant 1: LOW works as expected", {
  adlb_f <- adlb %>% filter(WGRLOVFL == "Y")

  # Please note the step below can be skipped if you are using DTYPE PHANTOM
  # you would use this adsl_adlb_merge_using_worst_flag

  # Create new grouping variables ATOXGR_GP, BTOXGR_GP
  adlb_out <- adlb_f %>%
    mutate(
      ATOXGR_GP = case_when(
        ATOXGR %in% c(0, 1, 2, 3, 4) ~ "Not Low",
        ATOXGR == -1 ~ "1",
        ATOXGR == -2 ~ "2",
        ATOXGR == -3 ~ "3",
        ATOXGR == -4 ~ "4",
        ATOXGR == "<Missing>" ~ "Missing"
      )
    ) %>%
    mutate(
      BTOXGR_GP = case_when(
        BTOXGR %in% c(0, 1, 2, 3, 4) ~ "Not Low",
        BTOXGR == -1 ~ "1",
        BTOXGR == -2 ~ "2",
        BTOXGR == -3 ~ "3",
        BTOXGR == -4 ~ "4",
        BTOXGR == "<Missing>" ~ "Missing"
      )
    )

  adlb_out <- adlb_out %>% mutate(
    AVISIT = forcats::fct_reorder(AVISIT, AVISITN),
    ATOXGR_GP = factor(ATOXGR_GP, levels = c("Not Low", "1", "2", "3", "4", "Missing")),
    BTOXGR_GP = factor(BTOXGR_GP, levels = c("Not Low", "1", "2", "3", "4", "Missing"))
  )

  adlb_out <- adlb_out %>%
    var_relabel(
      PARAMCD = "Parameter Code",
      AVISIT = "Visit",
      ATOXGR_GP = "NCI CTCAE Grade at Visit",
      BTOXGR_GP = "Baseline NCI CTCAE Grade"
    )

  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_rows_by(
      "PARAMCD",
      split_fun = drop_split_levels, label_pos = "topleft", split_label = obj_label(adlb_out$PARAMCD)
    ) %>%
    split_rows_by(
      "AVISIT",
      split_fun = drop_split_levels, label_pos = "topleft", split_label = obj_label(adlb_out$AVISIT)
    ) %>%
    split_rows_by(
      "ATOXGR_GP",
      split_fun = drop_split_levels, label_pos = "topleft", split_label = obj_label(adlb_out$ATOXGR_GP)
    ) %>%
    summarize_num_patients(var = "USUBJID", .stats = c("unique_count")) %>%
    count_occurrences("BTOXGR_GP", denom = "n", drop = TRUE) %>%
    append_varlabels(adlb_out, "BTOXGR_GP", indent = 3L) %>%
    build_table(df = adlb_out, alt_counts_df = adsl_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("LBT13 variant 2: HIGH works as expected", {
  adlb_f <- adlb %>% filter(WGRHIVFL == "Y")

  # Please note the step below can be skipped if you are using DTYPE PHANTOM
  adlb_out <- adsl_f %>%
    h_adsl_adlb_merge_using_worst_flag(
      adlb_f,
      worst_flag = c("WGRHIVFL" = "Y"),
      by_visit = TRUE,
      no_fillin_visits = c("SCREENING", "BASELINE", "UNSCHEDULED")
    )

  # Create new grouping variables ATOXGR_GP, BTOXGR_GP
  adlb_out <- adlb_out %>%
    mutate(
      ATOXGR_GP = case_when(
        ATOXGR %in% c(0, -1, -2, -3, -4) ~ "Not High",
        ATOXGR == 1 ~ "1",
        ATOXGR == 2 ~ "2",
        ATOXGR == 3 ~ "3",
        ATOXGR == 4 ~ "4",
        ATOXGR == "<Missing>" ~ "Missing"
      )
    ) %>%
    mutate(
      BTOXGR_GP = case_when(
        BTOXGR %in% c(0, -1, -2, -3, -4) ~ "Not High",
        BTOXGR == 1 ~ "1",
        BTOXGR == 2 ~ "2",
        BTOXGR == 3 ~ "3",
        BTOXGR == 4 ~ "4",
        BTOXGR == "<Missing>" ~ "Missing"
      )
    )

  adlb_out <- adlb_out %>% mutate(
    AVISIT = forcats::fct_reorder(AVISIT, AVISITN),
    ATOXGR_GP = factor(ATOXGR_GP, levels = c("Not High", "1", "2", "3", "4", "Missing")),
    BTOXGR_GP = factor(BTOXGR_GP, levels = c("Not High", "1", "2", "3", "4", "Missing"))
  )

  adlb_out <- adlb_out %>%
    var_relabel(
      PARAMCD = "Parameter Code",
      AVISIT = "Visit",
      ATOXGR_GP = "NCI CTCAE Grade at Visit",
      BTOXGR_GP = "Baseline NCI CTCAE Grade"
    )

  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_rows_by(
      "PARAMCD",
      split_fun = drop_split_levels, label_pos = "topleft", split_label = obj_label(adlb_out$PARAMCD)
    ) %>%
    split_rows_by(
      "AVISIT",
      split_fun = drop_split_levels, label_pos = "topleft", split_label = obj_label(adlb_out$AVISIT)
    ) %>%
    split_rows_by(
      "ATOXGR_GP",
      split_fun = drop_split_levels, label_pos = "topleft", split_label = obj_label(adlb_out$ATOXGR_GP)
    ) %>%
    summarize_num_patients(var = "USUBJID", .stats = c("unique_count")) %>%
    count_occurrences("BTOXGR_GP", denom = "n", drop = TRUE) %>%
    append_varlabels(adlb_out, "BTOXGR_GP", indent = 3L) %>%
    build_table(df = adlb_out, alt_counts_df = adsl_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("LBT13 variant 3: LOW without baseline missing works as expected", {
  adlb_f <- adlb %>% filter(WGRLOVFL == "Y")

  # Please note the step below can be skipped if you are using DTYPE PHANTOM
  adlb_out <- adsl_f %>%
    h_adsl_adlb_merge_using_worst_flag(
      adlb_f,
      worst_flag = c("WGRLOVFL" = "Y"),
      by_visit = TRUE,
      no_fillin_visits = c("SCREENING", "BASELINE", "UNSCHEDULED")
    )

  # Create new grouping variables ATOXGR_GP, BTOXGR_GP
  adlb_out <- adlb_out %>%
    filter(BTOXGR != "<Missing>") %>% # filter out missing baseline grade
    mutate(
      ATOXGR_GP = case_when(
        ATOXGR %in% c(0, 1, 2, 3, 4) ~ "Not Low",
        ATOXGR == -1 ~ "1",
        ATOXGR == -2 ~ "2",
        ATOXGR == -3 ~ "3",
        ATOXGR == -4 ~ "4",
        ATOXGR == "<Missing>" ~ "Missing"
      )
    ) %>%
    mutate(
      BTOXGR_GP = case_when(
        BTOXGR %in% c(0, 1, 2, 3, 4) ~ "Not Low",
        BTOXGR == -1 ~ "1",
        BTOXGR == -2 ~ "2",
        BTOXGR == -3 ~ "3",
        BTOXGR == -4 ~ "4"
      )
    )

  adlb_out <- adlb_out %>% mutate(
    AVISIT = forcats::fct_reorder(AVISIT, AVISITN),
    ATOXGR_GP = factor(ATOXGR_GP, levels = c("Not Low", "1", "2", "3", "4", "Missing")),
    BTOXGR_GP = factor(BTOXGR_GP, levels = c("Not Low", "1", "2", "3", "4"))
  )

  adlb_out <- adlb_out %>%
    var_relabel(
      PARAMCD = "Parameter Code",
      AVISIT = "Visit",
      ATOXGR_GP = "NCI CTCAE Grade at Visit",
      BTOXGR_GP = "Baseline NCI CTCAE Grade"
    )

  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_rows_by(
      "PARAMCD",
      split_fun = drop_split_levels, label_pos = "topleft", split_label = obj_label(adlb_out$PARAMCD)
    ) %>%
    split_rows_by(
      "AVISIT",
      split_fun = drop_split_levels, label_pos = "topleft", split_label = obj_label(adlb_out$AVISIT)
    ) %>%
    split_rows_by(
      "ATOXGR_GP",
      split_fun = drop_split_levels, label_pos = "topleft", split_label = obj_label(adlb_out$ATOXGR_GP)
    ) %>%
    summarize_num_patients(var = "USUBJID", .stats = c("unique_count")) %>%
    count_occurrences("BTOXGR_GP", denom = "n", drop = TRUE) %>%
    append_varlabels(adlb_out, "BTOXGR_GP", indent = 3L) %>%
    build_table(df = adlb_out, alt_counts_df = adsl_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("LBT13 variant 4: HIGH with missing baseline considered as grade 0 works as expected", { # nolint
  adlb_f <- adlb %>% filter(WGRHIVFL == "Y")

  # Please note the step below can be skipped if you are using DTYPE PHANTOM
  adlb_out <- adsl_f %>%
    h_adsl_adlb_merge_using_worst_flag(
      adlb_f,
      worst_flag = c("WGRHIVFL" = "Y"),
      by_visit = TRUE,
      no_fillin_visits = c("SCREENING", "BASELINE", "UNSCHEDULED")
    )

  # Create new grouping variables ATOXGR_GP, BTOXGR_GP
  adlb_out <- adlb_out %>%
    mutate(
      ATOXGR_GP = case_when(
        ATOXGR %in% c(0, -1, -2, -3, -4) ~ "Not High",
        ATOXGR == 1 ~ "1",
        ATOXGR == 2 ~ "2",
        ATOXGR == 3 ~ "3",
        ATOXGR == 4 ~ "4",
        ATOXGR == "<Missing>" ~ "Missing"
      )
    ) %>%
    mutate(
      BTOXGR_GP = case_when(
        BTOXGR %in% c(0, -1, -2, -3, -4, "<Missing>") ~ "Not High", # Missing BTOXGR now grouped to "Not High"
        BTOXGR == 1 ~ "1",
        BTOXGR == 2 ~ "2",
        BTOXGR == 3 ~ "3",
        BTOXGR == 4 ~ "4"
      )
    )

  adlb_out <- adlb_out %>% mutate(
    AVISIT = forcats::fct_reorder(AVISIT, AVISITN),
    ATOXGR_GP = factor(ATOXGR_GP, levels = c("Not High", "1", "2", "3", "4", "Missing")),
    BTOXGR_GP = factor(BTOXGR_GP, levels = c("Not High", "1", "2", "3", "4", "Missing"))
  )

  adlb_out <- adlb_out %>%
    var_relabel(
      PARAMCD = "Parameter Code",
      AVISIT = "Visit",
      ATOXGR_GP = "NCI CTCAE Grade at Visit",
      BTOXGR_GP = "Baseline NCI CTCAE Grade"
    )

  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_rows_by(
      "PARAMCD",
      split_fun = drop_split_levels, label_pos = "topleft", split_label = obj_label(adlb_out$PARAMCD)
    ) %>%
    split_rows_by(
      "AVISIT",
      split_fun = drop_split_levels, label_pos = "topleft", split_label = obj_label(adlb_out$AVISIT)
    ) %>%
    split_rows_by(
      "ATOXGR_GP",
      split_fun = drop_split_levels, label_pos = "topleft", split_label = obj_label(adlb_out$ATOXGR_GP)
    ) %>%
    summarize_num_patients(var = "USUBJID", .stats = c("unique_count")) %>%
    count_occurrences("BTOXGR_GP", denom = "n", drop = TRUE) %>%
    append_varlabels(adlb_out, "BTOXGR_GP", indent = 3L) %>%
    build_table(df = adlb_out, alt_counts_df = adsl_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("LBT14 variant 5: HIGH with filled in grades works as expected", {
  adlb_f <- adlb %>% filter(WGRHIVFL == "Y")

  # Please note the step below can be skipped if you are using DTYPE PHANTOM
  adlb_out <- adsl_f %>%
    h_adsl_adlb_merge_using_worst_flag(
      adlb_f,
      worst_flag = c("WGRHIVFL" = "Y"),
      by_visit = TRUE,
      no_fillin_visits = c("SCREENING", "BASELINE", "UNSCHEDULED")
    )

  # Create new grouping variables ATOXGR_GP, BTOXGR_GP
  adlb_out <- adlb_out %>%
    mutate(
      ATOXGR_GP = case_when(
        ATOXGR %in% c(0, -1, -2, -3, -4) ~ "Not High",
        ATOXGR == 1 ~ "1",
        ATOXGR == 2 ~ "2",
        ATOXGR == 3 ~ "3",
        ATOXGR == 4 ~ "4",
        ATOXGR == "<Missing>" ~ "Missing"
      )
    ) %>%
    mutate(
      BTOXGR_GP = case_when(
        BTOXGR %in% c(0, -1, -2, -3, -4) ~ "Not High",
        BTOXGR == 1 ~ "1",
        BTOXGR == 2 ~ "2",
        BTOXGR == 3 ~ "3",
        BTOXGR == 4 ~ "4",
        BTOXGR == "<Missing>" ~ "Missing"
      )
    )

  adlb_out <- adlb_out %>% mutate(
    AVISIT = forcats::fct_reorder(AVISIT, AVISITN),
    ATOXGR_GP = factor(ATOXGR_GP, levels = c("Not High", "1", "2", "3", "4", "Missing")),
    BTOXGR_GP = factor(BTOXGR_GP, levels = c("Not High", "1", "2", "3", "4", "Missing"))
  )

  adlb_out <- adlb_out %>%
    var_relabel(
      PARAMCD = "Parameter Code",
      AVISIT = "Visit",
      ATOXGR_GP = "NCI CTCAE Grade at Visit",
      BTOXGR_GP = "Baseline NCI CTCAE Grade"
    )

  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_rows_by(
      "PARAMCD",
      split_fun = drop_split_levels, label_pos = "topleft", split_label = obj_label(adlb_out$PARAMCD)
    ) %>%
    split_rows_by(
      "AVISIT",
      split_fun = drop_split_levels, label_pos = "topleft", split_label = obj_label(adlb_out$AVISIT)
    ) %>%
    split_rows_by(
      "ATOXGR_GP",
      split_fun = keep_split_levels(c("Not High", "1", "2", "3", "4", "Missing")),
      label_pos = "topleft",
      split_label = obj_label(adlb_out$ATOXGR_GP)
    ) %>%
    summarize_num_patients(var = "USUBJID", .stats = c("unique_count")) %>%
    count_occurrences("BTOXGR_GP", denom = "n", drop = FALSE) %>%
    append_varlabels(adlb_out, "BTOXGR_GP", indent = 3L) %>%
    build_table(df = adlb_out, alt_counts_df = adsl_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
