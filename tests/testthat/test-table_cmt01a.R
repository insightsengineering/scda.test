adsl <- adsl_raw
adcm <- adcm_raw

adcm$CMSEQ <- as.factor(adcm$CMSEQ)

adsl <- df_explicit_na(adsl)
adcm <- df_explicit_na(adcm)

adsl <- adsl %>%
  filter(SAFFL == "Y")

adcm <- adcm %>%
  filter(SAFFL == "Y" & ATIREL == "CONCOMITANT") %>%
  var_relabel(CMDECOD = "Other Treatment")

testthat::test_that("CMT01A variant 1 is produced correctly", {
  split_fun <- drop_split_levels

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
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
      "ATC2",
      child_labels = "visible",
      nested = FALSE,
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
    append_varlabels(adcm, "CMDECOD", indent = 1L)

  result <- build_table(lyt = lyt, df = adcm, alt_counts_df = adsl) %>%
    prune_table() %>%
    # Sort lowest level terms by descending frequency.
    sort_at_path(
      path = c("ATC2", "*", "CMDECOD"),
      scorefun = score_occurrences
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("CMT01A variant 2 is produced correctly", {
  split_fun <- drop_split_levels

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
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
    append_varlabels(adcm, "CMDECOD", indent = 1L)

  result <- build_table(lyt = lyt, df = adcm, alt_counts_df = adsl) %>%
    prune_table() %>%
    # Sort lowest level terms by descending frequency.
    sort_at_path(
      path = c("ATC1", "*", "CMDECOD"),
      scorefun = score_occurrences
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("CMT01A variant 3 is produced correctly", {
  split_fun <- drop_split_levels

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
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
      "ATC2",
      child_labels = "visible",
      nested = FALSE,
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
    append_varlabels(adcm, "CMDECOD", indent = 1L)

  result <- build_table(lyt = lyt, df = adcm, alt_counts_df = adsl) %>%
    prune_table() %>%
    sort_at_path(path = c("ATC2"), scorefun = cont_n_allcols) %>%
    sort_at_path(path = c("ATC2", "*", "CMDECOD"), scorefun = score_occurrences)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("CMT01A variant 4 is produced correctly", {
  split_fun <- drop_split_levels

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
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
      "ATC2",
      child_labels = "visible",
      nested = FALSE,
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adcm$ATC2)
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      count_by = "CMSEQ",
      .stats = c("unique"),
      .labels = c(
        unique = "Total number of patients with at least one treatment"
      )
    ) %>%
    count_occurrences(vars = "CMDECOD", .indent_mods = -1L) %>%
    append_varlabels(adcm, "CMDECOD", indent = 1L)

  result <- build_table(lyt = lyt, df = adcm, alt_counts_df = adsl) %>%
    prune_table() %>%
    # Sort lowest level terms by descending frequency.
    sort_at_path(
      path = c("ATC2", "*", "CMDECOD"),
      scorefun = score_occurrences
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
