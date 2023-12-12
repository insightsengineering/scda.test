adsl <- adsl_raw
adae <- adae_raw

adsl <- df_explicit_na(adsl) %>% filter(TRT01A != "<Missing>")
adae <- df_explicit_na(adae) %>%
  var_relabel(
    AEBODSYS = "MedDRA System Organ Class",
    AEDECOD = "MedDRA Preferred Term"
  ) %>%
  filter(
    ANL01FL == "Y",
    AETOXGR != "<Missing>"
  )

# Pre-Processing
grade_groups <- list(
  "Grade 1-2" = c("1", "2"),
  "Grade 3-4" = c("3", "4"),
  "Grade 5" = "5"
)
adae$TOTAL_VAR <- "- Any adverse events - "

# Helper function to avoid filtering also the first part of the table, where general information is given.
my_row_condition <- function(row_fnc_condition) {
  function(table_row) {
    if (indent_mod(table_row) == 0) {
      return(TRUE)
    } else {
      row_fnc_condition(table_row)
    }
  }
}

# Helper function to calculate sum from first nested row
score_all_sum <- function(tt) {
  cleaf <- collect_leaves(tt)[[1]]
  if (NROW(cleaf) == 0) {
    stop("score_all_sum score function used at subtable [", obj_name(tt), "] that has no content.")
  }
  sum(sapply(row_values(cleaf), function(cv) cv[1]))
}

# Raw table used by variant 8/10
raw_table <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  split_rows_by(
    var = "TOTAL_VAR",
    label_pos = "hidden",
    child_labels = "visible",
    indent_mod = -1L
  ) %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = "unique",
    .labels = "- Any Grade -",
    .indent_mods = 7L
  ) %>%
  count_occurrences_by_grade(
    var = "AETOXGR",
    grade_groups = grade_groups,
    .indent_mods = 6L
  ) %>%
  split_rows_by(
    "AEBODSYS",
    child_labels = "visible",
    nested = FALSE,
    split_fun = drop_split_levels,
    split_label = var_labels(adae)[["AEBODSYS"]],
    label_pos = "topleft"
  ) %>%
  split_rows_by(
    "AEDECOD",
    child_labels = "visible",
    split_fun = add_overall_level("- Overall -", trim = TRUE),
    split_label = var_labels(adae)[["AEDECOD"]],
    label_pos = "topleft"
  ) %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = "unique",
    .labels = "- Any Grade -",
    .indent_mods = 6L
  ) %>%
  count_occurrences_by_grade(
    var = "AETOXGR",
    grade_groups = grade_groups,
    .indent_mods = 5L
  ) %>%
  append_topleft("                            Grade") %>%
  build_table(adae, alt_counts_df = adsl) %>%
  prune_table() %>%
  sort_at_path(
    path = "AEBODSYS",
    scorefun = score_all_sum,
    decreasing = TRUE
  ) %>%
  sort_at_path(
    path = c("AEBODSYS", "*", "AEDECOD"),
    scorefun = score_all_sum,
    decreasing = TRUE
  )

testthat::test_that("AET04 variant 1 is produced correctly", {
  res <- testthat::expect_silent(raw_table)

  testthat::expect_snapshot(res)

  # Pagination also works (and sorting)
  testthat::expect_silent(
    pag_result <- paginate_table(res, lpp = 15)
  )

  testthat::expect_identical(
    to_string_matrix(pag_result[[3]], with_spaces = FALSE)[4, 1],
    "cl A.1"
  )
  testthat::expect_identical(
    trimws(to_string_matrix(pag_result[[1]], with_spaces = FALSE)[5:6, 1]),
    c("- Any Grade -", "Grade 1-2")
  )
})

testthat::test_that("AET04 variant 2 is produced correctly (Fill in of Treatment Groups)", {
  adae2 <- adae %>% filter(ACTARM == "A: Drug X")

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_rows_by(
      var = "TOTAL_VAR",
      label_pos = "hidden",
      child_labels = "visible",
      indent_mod = -1L
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "- Any Grade -",
      .indent_mods = 7L
    ) %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = grade_groups,
      .indent_mods = 6L
    ) %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      split_fun = drop_split_levels,
      split_label = var_labels(adae)[["AEBODSYS"]],
      label_pos = "topleft"
    ) %>%
    split_rows_by(
      "AEDECOD",
      child_labels = "visible",
      split_fun = add_overall_level("- Overall -", trim = TRUE),
      split_label = var_labels(adae)[["AEDECOD"]],
      label_pos = "topleft"
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "- Any Grade -",
      .indent_mods = 6L
    ) %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = grade_groups,
      .indent_mods = 5L
    ) %>%
    append_topleft("                            Grade")

  result <- lyt %>%
    build_table(adae2, alt_counts_df = adsl) %>%
    prune_table() %>%
    sort_at_path(
      path = "AEBODSYS",
      scorefun = score_all_sum,
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = score_all_sum,
      decreasing = TRUE
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Pagination also works (and sorting)
  testthat::expect_silent(
    pag_result <- paginate_table(result, lpp = 15)
  )

  testthat::expect_identical(
    to_string_matrix(pag_result[[3]], with_spaces = FALSE)[4, 1],
    "cl A.1"
  )
  testthat::expect_identical(
    to_string_matrix(pag_result[[1]], with_spaces = FALSE)[5:6, 2],
    c("100 (74.6%)", "10 (7.5%)")
  )
})

testthat::test_that("AET04 variant 3 is produced correctly (Fill in of Grades)", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_rows_by(
      var = "TOTAL_VAR",
      label_pos = "hidden",
      child_labels = "visible",
      indent_mod = -1L
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "- Any Grade -",
      .indent_mods = 7L
    ) %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = grade_groups,
      .indent_mods = 6L
    ) %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      split_fun = drop_split_levels,
      split_label = var_labels(adae)[["AEBODSYS"]],
      label_pos = "topleft"
    ) %>%
    split_rows_by(
      "AEDECOD",
      child_labels = "visible",
      split_fun = add_overall_level("- Overall -", trim = TRUE),
      split_label = var_labels(adae)[["AEDECOD"]],
      label_pos = "topleft"
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "- Any Grade -",
      .indent_mods = 6L
    ) %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = grade_groups,
      .indent_mods = 5L
    ) %>%
    append_topleft("                            Grade")

  result <- lyt %>%
    build_table(adae, alt_counts_df = adsl) %>%
    sort_at_path(
      path = "AEBODSYS",
      scorefun = score_all_sum,
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = score_all_sum,
      decreasing = TRUE
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Pagination also works (and sorting)
  testthat::expect_silent(
    pag_result <- paginate_table(result, lpp = 15)
  )

  testthat::expect_identical(
    to_string_matrix(pag_result[[3]], with_spaces = FALSE)[4, 1],
    "cl A.1"
  )
  testthat::expect_identical(
    to_string_matrix(pag_result[[1]], with_spaces = FALSE)[5:6, 2],
    c("100 (74.6%)", "10 (7.5%)")
  )
})

testthat::test_that("AET04 variant 4 is produced correctly (Collapsing of Grades: grades 1&2, grades 3&4&5)", {
  grade_groups_1 <- list(
    "Grade 1-2" = c("1", "2"),
    "Grade 3-5" = c("3", "4", "5")
  )

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_rows_by(
      var = "TOTAL_VAR",
      label_pos = "hidden",
      child_labels = "visible",
      indent_mod = -1L
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "- Any Grade -",
      .indent_mods = 7L
    ) %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = grade_groups_1,
      .indent_mods = 6L
    ) %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      split_fun = drop_split_levels,
      split_label = var_labels(adae)[["AEBODSYS"]],
      label_pos = "topleft"
    ) %>%
    split_rows_by(
      "AEDECOD",
      child_labels = "visible",
      split_fun = add_overall_level("- Overall -", trim = TRUE),
      split_label = var_labels(adae)[["AEDECOD"]],
      label_pos = "topleft"
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "- Any Grade -",
      .indent_mods = 6L
    ) %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = grade_groups_1,
      .indent_mods = 5L
    ) %>%
    append_topleft("                            Grade")

  result <- lyt %>%
    build_table(adae, alt_counts_df = adsl) %>%
    prune_table() %>%
    sort_at_path(
      path = "AEBODSYS",
      scorefun = score_all_sum,
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = score_all_sum,
      decreasing = TRUE
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Pagination works
  testthat::expect_silent(
    pag_result <- paginate_table(result, lpp = 15)
  )
})

# No test done for variant 5 (Using Worst Grade Flags from ADAE) since it's similar to
# variant 1 (just pre-processing the data).

testthat::test_that(
  "AET04 variant 6 is produced correctly (with an Incidence Rate of at Least 5%, totals restricted)",
  {
    # Simple wrapper to return subset ADAE to a threshold of xx%.
    get_adae_trimmed <- function(adsl, adae, cutoff_rate) {
      n_per_arm <- adsl %>%
        dplyr::count(ACTARM)

      anl_terms <- adae %>%
        dplyr::group_by(ACTARM, AEBODSYS, AEDECOD) %>%
        dplyr::count(
          unique_terms = n_distinct(USUBJID)
        ) %>%
        dplyr::select(-n) %>%
        dplyr::ungroup()

      anl_terms <- dplyr::left_join(
        anl_terms,
        n_per_arm,
        by = "ACTARM"
      ) %>%
        dplyr::mutate(
          ae_rate = unique_terms / n
        ) %>%
        dplyr::filter(ae_rate >= cutoff_rate) %>%
        dplyr::select(AEDECOD) %>%
        unique()

      anl <- dplyr::left_join(
        anl_terms,
        adae,
        by = "AEDECOD"
      )
      anl
    }

    adae6 <- get_adae_trimmed(adsl, adae, cutoff_rate = 0.4)

    lyt <- basic_table(show_colcounts = TRUE) %>%
      split_cols_by("ACTARM") %>%
      split_rows_by(
        var = "TOTAL_VAR",
        label_pos = "hidden",
        child_labels = "visible",
        indent_mod = -1L
      ) %>%
      summarize_num_patients(
        var = "USUBJID",
        .stats = "unique",
        .labels = "- Any Grade -",
        .indent_mods = 7L
      ) %>%
      count_occurrences_by_grade(
        var = "AETOXGR",
        grade_groups = grade_groups,
        .indent_mods = 6L
      ) %>%
      split_rows_by(
        "AEBODSYS",
        child_labels = "visible",
        nested = FALSE,
        split_fun = drop_split_levels,
        split_label = var_labels(adae)[["AEBODSYS"]],
        label_pos = "topleft"
      ) %>%
      split_rows_by(
        "AEDECOD",
        child_labels = "visible",
        split_fun = add_overall_level("- Overall -", trim = TRUE),
        split_label = var_labels(adae)[["AEDECOD"]],
        label_pos = "topleft"
      ) %>%
      summarize_num_patients(
        var = "USUBJID",
        .stats = "unique",
        .labels = "- Any Grade -",
        .indent_mods = 6L
      ) %>%
      count_occurrences_by_grade(
        var = "AETOXGR",
        grade_groups = grade_groups,
        .indent_mods = 5L
      ) %>%
      append_topleft("                            Grade")

    result <- lyt %>%
      build_table(adae6, alt_counts_df = adsl) %>%
      prune_table() %>%
      sort_at_path(
        path = "AEBODSYS",
        scorefun = score_all_sum,
        decreasing = TRUE
      ) %>%
      sort_at_path(
        path = c("AEBODSYS", "*", "AEDECOD"),
        scorefun = score_all_sum,
        decreasing = TRUE
      )

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)

    # Pagination works
    testthat::expect_silent(
      pag_result <- paginate_table(result, lpp = 15)
    )
  }
)

# No test done for variant 7, Adverse Events by Highest NCI CTCAE Grade
# (with an Incidence Rate of at Least X Patients, totals unrestriced).
# With this variant, the SOC level is not trimmed (even if there are no terms left).

# NOTE: STREAM logic will only trim at term level
testthat::test_that("AET04 variant 8 is produced correctly (with an Incidence Rate of at Least X Patients)", {
  cutoff <- 58L
  row_condition <- has_count_in_any_col(atleast = cutoff, col_names = levels(adsl$ACTARM))

  result <- prune_table(raw_table, keep_content_rows(my_row_condition(row_condition)))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Pagination works
  testthat::expect_silent(
    pag_result <- paginate_table(result, lpp = 15)
  )
})

# NOTE: STREAM logic will only stream at term level
testthat::test_that("AET04 variant 9 is produced correctly (with a Difference in Incidence Rate of at Least X%)", {
  cutoff <- 0.1
  row_condition <- has_fractions_difference(atleast = cutoff, col_names = levels(adsl$ACTARM))

  result <- prune_table(raw_table, keep_content_rows(my_row_condition(row_condition)))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Pagination works
  testthat::expect_silent(
    pag_result <- paginate_table(result, lpp = 15)
  )
})

# No test done for variant 10, Adverse Events by Highest NCI CTCAE Grade
# (with an Incidence Rate of at Least X%, SOCs below X% removed).
# With this variant, SOC levels above the threshold are still in the table even if
# there are no terms left.

testthat::test_that(
  "AET04 variant 11 is produced correctly (with Incidence Rate of at Least X%, all SOCs w/o preferred terms removed)",
  {
    cutoff <- 0.4
    row_condition <- has_fraction_in_any_col(atleast = cutoff, col_names = levels(adsl$ACTARM))

    result <- prune_table(raw_table, keep_content_rows(my_row_condition(row_condition)))

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)

    # Pagination works
    testthat::expect_silent(
      pag_result <- paginate_table(result, lpp = 15)
    )
  }
)
