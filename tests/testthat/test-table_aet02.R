# Test all variants of AET02

adsl <- adsl_raw
adae <- adae_raw

# Ensure character variables are converted to factors and empty strings and NAs are explicit missing levels.
adsl <- df_explicit_na(adsl)
adae <- df_explicit_na(adae) %>%
  var_relabel(
    AEBODSYS = "MedDRA System Organ Class",
    AEDECOD = "MedDRA Preferred Term"
  ) %>%
  filter(ANL01FL == "Y")

# Define the split function
split_fun <- drop_split_levels

testthat::test_that("AET02 variant 1 is produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ACTARM") %>%
    add_overall_col(label = "All Patients") %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Overall total number of events"
      )
    ) %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adae$AEBODSYS)
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Total number of events"
      )
    ) %>%
    count_occurrences(
      vars = "AEDECOD",
      .indent_mods = -1L
    ) %>%
    append_varlabels(adae, "AEDECOD", indent = 1L)

  result <- build_table(lyt, df = adae, alt_counts_df = adsl)

  result <- result %>%
    prune_table() %>%
    sort_at_path(
      path = c("AEBODSYS"),
      scorefun = cont_n_allcols
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = score_occurrences
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Testing pagination with not repeated Total number of patients
  pag_result <- paginate_table(result, cpp = 75) # std is 70 which fails
  testthat::expect_identical(
    to_string_matrix(pag_result[[1]])[3, 1],
    "Total number of patients with at least one adverse event"
  )
  testthat::expect_identical(to_string_matrix(pag_result[[2]])[5, 1], "cl A.1")
})

testthat::test_that("AET02 variant 2 is produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ACTARM") %>%
    add_overall_col(label = "All Patients") %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Overall total number of events"
      )
    ) %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adae$AEBODSYS)
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event"
      )
    ) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = -1L) %>%
    append_varlabels(adae, "AEDECOD", indent = 1L) %>%
    count_values(
      "STUDYID",
      values = as.character(unique(adae$STUDYID)),
      .stats = "count",
      .labels = c(count = "Total number of events"),
      .indent_mods = c(count = -1L)
    )

  result <- build_table(lyt, df = adae, alt_counts_df = adsl) %>%
    prune_table() %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = score_occurrences
    ) %>%
    sort_at_path(
      path = c("AEBODSYS"),
      scorefun = cont_n_allcols
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET02 variant 3 is produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ACTARM") %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Overall total number of events"
      )
    ) %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adae$AEBODSYS)
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Total number of events"
      )
    ) %>%
    split_rows_by(
      "AEHLT",
      child_labels = "visible",
      nested = TRUE,
      split_fun = split_fun,
      indent_mod = -1L,
      label_pos = "topleft",
      split_label = obj_label(adae$AEHLT)
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Total number of events"
      )
    ) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = c(count_fraction = -1L)) %>%
    append_varlabels(adae, c("AEDECOD"), indent = 2L)

  result <- build_table(lyt, df = adae, alt_counts_df = adsl) %>%
    prune_table() %>%
    sort_at_path(
      path = c("AEBODSYS"),
      scorefun = cont_n_allcols
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEHLT"),
      scorefun = cont_n_allcols
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEHLT", "*", "AEDECOD"),
      scorefun = score_occurrences
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET02 variant 4 is produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ACTARM") %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Total number of events"
      ),
      show_labels = "hidden"
    ) %>%
    count_occurrences(vars = "AEDECOD") %>%
    append_varlabels(adae, "AEDECOD")

  result <- build_table(lyt, df = adae, alt_counts_df = adsl) %>%
    sort_at_path(
      path = c("AEDECOD"),
      scorefun = score_occurrences
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET02 variant 5 is produced correctly", {
  adae_5 <- adae %>% dplyr::filter(ACTARM != "C: Combination")

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ACTARM") %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Overall total number of events"
      )
    ) %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adae$AEBODSYS)
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Total number of events"
      )
    ) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = c(count_fraction = -1L)) %>%
    append_varlabels(adae, "AEDECOD", indent = 1L)

  result <- build_table(lyt, df = adae_5, alt_counts_df = adsl) %>%
    prune_table() %>%
    sort_at_path(
      path = c("AEBODSYS"),
      scorefun = cont_n_allcols
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = score_occurrences
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET02 variant 6 is produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ACTARM") %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      indent_mod = -1L,
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adae$AEBODSYS)
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "Total number of patients with at least one adverse event"
    ) %>%
    count_occurrences(
      vars = "AEDECOD",
      .indent_mods = c(count_fraction = 1L)
    ) %>%
    append_varlabels(adae, "AEDECOD", indent = 1L)

  result <- build_table(lyt, df = adae, alt_counts_df = adsl) %>%
    prune_table() %>%
    sort_at_path(
      path = c("AEBODSYS"),
      scorefun = cont_n_allcols
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = score_occurrences
    )

  criteria_fun <- function(tr) is(tr, "ContentRow")
  result <- trim_rows(result, criteria = criteria_fun)

  row_condition <- has_fraction_in_any_col(
    atleast = 0.05,
    col_names = names(table(adsl$ACTARM))
  )
  result <- prune_table(result, keep_rows(row_condition))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET02 variant 7 is produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ACTARM") %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = TRUE,
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adae$AEBODSYS)
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "Total number of patients with at least one adverse event"
    ) %>%
    split_rows_by(
      "AEHLT",
      child_labels = "visible",
      nested = TRUE,
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adae$AEHLT)
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "Total number of patients with at least one adverse event"
    ) %>%
    count_occurrences(vars = "AEDECOD") %>%
    append_varlabels(adae, c("AEDECOD"), indent = 2L)

  result <- build_table(lyt, adae, alt_counts_df = adsl) %>%
    prune_table() %>%
    sort_at_path(
      path = c("AEBODSYS"),
      scorefun = cont_n_allcols,
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEHLT"),
      scorefun = cont_n_allcols,
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEHLT", "*", "AEDECOD"),
      scorefun = score_occurrences,
      decreasing = TRUE
    )

  criteria_fun <- function(tr) is(tr, "ContentRow")
  result <- trim_rows(result, criteria = criteria_fun)

  row_condition <- has_fraction_in_any_col(
    atleast = 0.05,
    col_names = names(table(adsl$ACTARM))
  )
  result <- prune_table(result, keep_rows(row_condition))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET02 variant 8 is produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ACTARM") %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      indent_mod = -1L,
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adae$AEBODSYS)
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "Total number of patients with at least one adverse event"
    ) %>%
    count_occurrences(
      vars = "AEDECOD",
      .indent_mods = c(count_fraction = 1L)
    ) %>%
    append_varlabels(adae, "AEDECOD", indent = 1L)

  result <- build_table(lyt, df = adae, alt_counts_df = adsl) %>%
    prune_table() %>%
    sort_at_path(
      path = c("AEBODSYS"),
      scorefun = cont_n_allcols
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = score_occurrences
    )

  criteria_fun <- function(tr) is(tr, "ContentRow")
  result <- trim_rows(result, criteria = criteria_fun)

  row_condition <- has_fraction_in_any_col(
    atleast = 0.10,
    col_names = names(table(adsl$ACTARM))
  )
  result <- prune_table(result, keep_rows(row_condition))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET02 variant 9 is produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ACTARM") %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      indent_mod = -1L,
      split_fun = split_fun,
      split_label = obj_label(adae$AEBODSYS),
      label_pos = "topleft"
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "Total number of patients with at least one adverse event"
    ) %>%
    count_occurrences(
      vars = "AEDECOD",
      .indent_mods = c(count_fraction = 1L)
    ) %>%
    append_varlabels(adae, "AEDECOD", indent = 1L)

  result <- build_table(lyt, df = adae, alt_counts_df = adsl) %>%
    prune_table() %>%
    sort_at_path(
      path = c("AEBODSYS"),
      scorefun = cont_n_allcols
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = score_occurrences
    )

  criteria_fun <- function(tr) is(tr, "ContentRow")
  result <- trim_rows(result, criteria = criteria_fun)

  row_condition <- has_count_in_any_col(
    atleast = 3,
    col_names = names(table(adsl$ACTARM))
  )
  result <- prune_table(result, keep_rows(row_condition))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET02 variant 10 is produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ACTARM") %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      indent_mod = -1L,
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adae$AEBODSYS)
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "Total number of patients with at least one adverse event"
    ) %>%
    count_occurrences(
      vars = "AEDECOD",
      .indent_mods = c(count_fraction = 1L)
    ) %>%
    append_varlabels(adae, "AEDECOD", indent = 1L)

  result <- build_table(lyt, df = adae, alt_counts_df = adsl) %>%
    prune_table() %>%
    sort_at_path(
      path = c("AEBODSYS"),
      scorefun = cont_n_allcols
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = score_occurrences
    )

  criteria_fun <- function(tr) is(tr, "ContentRow")
  result <- trim_rows(result, criteria = criteria_fun)

  row_condition <- has_fractions_difference(
    atleast = 0.05,
    col_names = levels(adsl$ACTARM)
  )
  result <- prune_table(result, keep_rows(row_condition))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET02 variant 11 is produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ACTARM") %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      indent_mod = -1L,
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adae$AEBODSYS)
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "Total number of patients with at least one adverse event"
    ) %>%
    count_occurrences(
      vars = "AEDECOD",
      .indent_mods = c(count_fraction = 1L)
    ) %>%
    append_varlabels(adae, "AEDECOD", indent = 1L)

  result <- build_table(lyt, df = adae, alt_counts_df = adsl) %>%
    prune_table() %>%
    sort_at_path(
      path = c("AEBODSYS"),
      scorefun = cont_n_allcols
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = score_occurrences
    )

  criteria_fun <- function(tr) is(tr, "ContentRow")
  result <- trim_rows(result, criteria = criteria_fun)

  row_condition <- has_fraction_in_cols(
    atleast = 0.05,
    col_names = c("B: Placebo")
  )
  result <- prune_table(result, keep_rows(row_condition))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET02 variant 12 is produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ACTARM") %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      indent_mod = -1L,
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adae$AEBODSYS)
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "Total number of patients with at least one adverse event"
    ) %>%
    count_occurrences(
      vars = "AEDECOD",
      .indent_mods = c(count_fraction = 1L)
    ) %>%
    append_varlabels(adae, "AEDECOD", indent = 1L)

  result <- build_table(lyt, df = adae, alt_counts_df = adsl) %>%
    prune_table() %>%
    sort_at_path(
      path = c("AEBODSYS"),
      scorefun = cont_n_allcols
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = score_occurrences
    )

  criteria_fun <- function(tr) is(tr, "ContentRow")
  result <- trim_rows(result, criteria = criteria_fun)

  row_condition1 <- has_fractions_difference(atleast = 0.05, col_names = c("A: Drug X", "B: Placebo"))
  row_condition2 <- has_fractions_difference(atleast = 0.05, col_names = c("A: Drug X", "C: Combination"))
  row_condition <- row_condition1 | row_condition2
  result <- prune_table(result, keep_rows(row_condition))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
