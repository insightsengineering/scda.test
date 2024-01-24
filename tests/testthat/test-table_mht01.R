# Tests the single variant for MHT01

adsl <- adsl_raw
admh <- admh_raw

adsl <- df_explicit_na(adsl)
admh <- df_explicit_na(admh)

adsl_f <- adsl %>%
  filter(SAFFL == "Y") %>%
  select(USUBJID, ACTARM)

admh_f <- admh %>%
  filter(SAFFL == "Y" & MHBODSYS != "" & MHDECOD != "") %>%
  var_relabel(
    MHBODSYS = "MedDRA System Organ Class",
    MHDECOD = "MedDRA Preferred Term"
  )

split_fun <- drop_split_levels

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  analyze_num_patients(
    vars = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c("Total number of patients with at least one condition", "Total number of conditions")
  ) %>%
  split_rows_by(
    var = "MHBODSYS",
    split_fun = split_fun,
    label_pos = "topleft",
    split_label = obj_label(admh_f$MHBODSYS)
  ) %>%
  analyze_num_patients(
    vars = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c("Total number of patients with at least one condition", "Total number of conditions"),
    show_labels = "hidden"
  ) %>%
  count_occurrences(vars = "MHDECOD") %>%
  append_varlabels(admh_f, "MHDECOD", indent = 1L)

testthat::test_that("MHT01 variant 1 is produced accurately", {
  result <- build_table(lyt, admh_f, alt_counts_df = adsl_f) %>%
    prune_table()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("MHT01 variant 2 is produced accurately", {
  admh_f_prior <- admh_f %>%
    filter(ASTDY <= 0)

  result <- build_table(lyt, admh_f_prior, alt_counts_df = adsl_f) %>%
    prune_table()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("MHT01 variant 3 is produced accurately", {
  split_fun <- drop_split_levels

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique"),
      .labels = c("Total number of patients with at least one condition")
    ) %>%
    split_rows_by(
      var = "MHBODSYS",
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(admh_f$MHBODSYS)
    ) %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique"),
      .labels = c("Total number of patients with at least one condition"),
      show_labels = "hidden"
    ) %>%
    count_occurrences(vars = "MHDECOD") %>%
    append_varlabels(admh_f, "MHDECOD", indent = 1L)

  result <- build_table(lyt, admh_f, alt_counts_df = adsl) %>%
    prune_table()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("MHT01 variant 5 is produced accurately", {
  split_fun <- drop_split_levels

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    add_overall_col("All Patients") %>%
    analyze_num_patients(
      "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(unique = "Total number of patients with at least one event", nonunique = "Total number of conditions")
    ) %>%
    split_rows_by(
      var = "MHBODSYS",
      split_fun = split_fun,
      child_labels = "visible",
      label_pos = "topleft",
      split_label = obj_label(admh_f$MHBODSYS)
    ) %>%
    summarize_num_patients(
      "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(unique = "Total number of patients with at least one event", nonunique = "Total number of conditions")
    ) %>%
    count_occurrences(vars = "MHDECOD", .indent_mods = -1L) %>%
    append_varlabels(admh_f, "MHDECOD", indent = 1L)

  scorefun_hlt <- cont_n_allcols
  scorefun_llt <- score_occurrences_cols(col_indices = nlevels(adsl_f$ACTARM) + 1)

  result <- build_table(lyt, admh_f, alt_counts_df = adsl_f) %>%
    prune_table() %>%
    sort_at_path(path = c("MHBODSYS"), scorefun = scorefun_hlt) %>%
    sort_at_path(path = c("MHBODSYS", "*", "MHDECOD"), scorefun = scorefun_llt)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
