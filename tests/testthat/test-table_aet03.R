adsl <- adsl_pharmaverse
adae <- adae_pharmaverse

# Ensure character variables are converted to factors and empty strings and NAs are explicit missing levels.
adsl <- df_explicit_na(adsl)
adae <- df_explicit_na(adae) %>%
  var_relabel(
    AEBODSYS = "MedDRA System Organ Class",
    AEDECOD = "MedDRA Preferred Term"
  ) %>%
  filter(ANL01FL == "Y")

adae <- adae %>% mutate(ASEV = as.character(AESEV))
adae$ASEV[1:15] <- "LIFE THREATENING"
adae <- adae %>% mutate(ASEV = factor(ASEV, levels = c("MILD", "MODERATE", "SEVERE", "LIFE THREATENING")))

testthat::test_that("AET03 variant 1 is produced correctly", {
  grade_groups <- list("- Any Intensity -" = c("MILD", "MODERATE", "SEVERE", "LIFE THREATENING"))

  split_fun <- trim_levels_in_group

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    count_occurrences_by_grade(
      var = "ASEV",
      grade_groups = grade_groups
    ) %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = TRUE,
      split_fun = split_fun("ASEV"),
      label_pos = "topleft",
      split_label = obj_label(adae$AEBODSYS)
    ) %>%
    summarize_occurrences_by_grade(
      var = "ASEV",
      grade_groups = grade_groups
    ) %>%
    split_rows_by(
      "AEDECOD",
      child_labels = "visible",
      nested = TRUE,
      indent_mod = -1L,
      split_fun = split_fun("ASEV"),
      label_pos = "topleft",
      split_label = obj_label(adae$AEDECOD)
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = c("- Any Intensity -")
    ) %>%
    count_occurrences_by_grade(
      var = "ASEV",
      .indent_mods = -1L
    ) %>%
    append_varlabels(adae, "AESEV", indent = 2L)

  result <- lyt %>%
    build_table(
      adae,
      alt_counts_df = adsl
    ) %>%
    sort_at_path(
      path = "AEBODSYS",
      scorefun = cont_n_allcols,
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = cont_n_allcols,
      decreasing = TRUE
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  result_matrix <- to_string_matrix(result, with_spaces = FALSE)

  # Pagination also works (and sorting)
  lpp_test <- 18
  testthat::expect_equal(
    nrow(paginate_table(result, lpp = lpp_test)[[1]]) + 4, # 4 is the header
    lpp_test
  )

  # With 8, it works perfectly for the first block but others have much more nesting
  lpp_test <- 8
  testthat::expect_error(
    suppressMessages(paginate_table(result, lpp = lpp_test)[[1]])
  )

  pag_result <- paginate_table(result, lpp = 16)

  testthat::expect_identical(
    to_string_matrix(pag_result[[3]], with_spaces = FALSE)[3, 1],
    "    Severity/Intensity"
  )
  testthat::expect_identical(
    to_string_matrix(pag_result[[1]], with_spaces = FALSE)[3:4, 1],
    c("    Severity/Intensity", "- Any Intensity -")
  )
})
