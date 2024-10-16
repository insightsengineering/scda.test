adsl <- adsl_pharmaverse
adcm <- adcm_pharmaverse

adcm$CMSEQ <- as.factor(adcm$CMSEQ)

# Ensure character variables are converted to factors and empty strings and NAs are explicit missing levels.
adsl <- df_explicit_na(adsl)
adcm <- df_explicit_na(adcm)

# Keep only safety-evaluable patients and concomitant medications
adsl <- adsl %>%
  filter(SAFFL == "Y")

adcm <- adcm %>%
  filter(SAFFL == "Y" & ATIREL == "CONCOMITANT")

testthat::test_that("CMT01B variant 1 is produced correctly", {
  split_fun <- drop_split_levels

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    analyze_num_patients(
      vars = "USUBJID",
      count_by = "CMSEQ",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one treatment",
        nonunique = "Total number of treatments"
      )
    ) %>%
    split_rows_by(
      "ATC1",
      child_labels = "visible",
      nested = FALSE,
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adcm$ATC1)
    ) %>%
    split_rows_by(
      "ATC2",
      child_labels = "visible",
      nested = TRUE,
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adcm$ATC2)
    ) %>%
    split_rows_by(
      "ATC3",
      child_labels = "visible",
      nested = TRUE,
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adcm$ATC3)
    ) %>%
    split_rows_by(
      "ATC4",
      child_labels = "visible",
      nested = TRUE,
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adcm$ATC4)
    ) %>%
    append_topleft("        Other Treatment") %>%
    summarize_num_patients(
      var = "USUBJID",
      count_by = "CMSEQ",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one treatment",
        nonunique = "Total number of treatments"
      )
    ) %>%
    count_occurrences(vars = "CMDECOD", .indent_mods = -1L)

  result <- build_table(
    lyt = lyt,
    df = adcm,
    alt_counts_df = adsl
  ) %>%
    prune_table() %>%
    # Sort lowest level terms by descending frequency.
    sort_at_path(
      path = c("ATC1", "*", "ATC2", "*", "ATC3", "*", "ATC4", "*", "CMDECOD"),
      scorefun = score_occurrences
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("CMT01B variant 2 is produced correctly", {
  split_fun <- drop_split_levels

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    analyze_num_patients(
      vars = "USUBJID",
      count_by = "CMSEQ",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one treatment",
        nonunique = "Total number of treatments"
      )
    ) %>%
    split_rows_by(
      "ATC3",
      child_labels = "visible",
      nested = FALSE,
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adcm$ATC3)
    ) %>%
    split_rows_by(
      "ATC2",
      child_labels = "visible",
      nested = TRUE,
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adcm$ATC2)
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      count_by = "CMSEQ",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one treatment",
        nonunique = "Total number of treatments"
      )
    ) %>%
    count_occurrences(vars = "CMDECOD", .indent_mods = -1L) %>%
    append_topleft("    Other Treatment")

  result <- build_table(
    lyt = lyt,
    df = adcm,
    alt_counts_df = adsl
  ) %>%
    prune_table() %>%
    # Sort lowest level terms by descending frequency.
    sort_at_path(
      path = c("ATC3", "*", "ATC2", "*", "CMDECOD"),
      scorefun = score_occurrences
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("CMT01B variant 4 is produced correctly", {
  split_fun <- drop_split_levels

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    split_rows_by(
      "ATC1",
      child_labels = "visible",
      nested = FALSE,
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adcm$ATC1)
    ) %>%
    split_rows_by(
      "ATC2",
      child_labels = "visible",
      nested = TRUE,
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adcm$ATC2)
    ) %>%
    split_rows_by(
      "ATC3",
      child_labels = "visible",
      nested = TRUE,
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adcm$ATC3)
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      count_by = "CMSEQ",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one treatment",
        nonunique = "Total number of treatments"
      )
    ) %>%
    split_rows_by(
      "ATC4",
      child_labels = "visible",
      nested = TRUE,
      indent_mod = -1L,
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adcm$ATC4)
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique"),
      .labels = c(
        unique = "Total number of patients with at least one treatment"
      )
    ) %>%
    count_occurrences(vars = "CMDECOD", .indent_mods = -1L) %>%
    append_topleft("        Other Treatment")

  result <- build_table(
    lyt = lyt,
    df = adcm,
    alt_counts_df = adsl
  ) %>%
    prune_table() %>%
    # Sort lowest level terms by descending frequency.
    sort_at_path(
      path = c("ATC1", "*", "ATC2", "*", "ATC3", "*", "ATC4", "*", "CMDECOD"),
      scorefun = score_occurrences
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
