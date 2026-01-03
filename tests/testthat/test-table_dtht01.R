# Test variants of DTHT01

adsl <- adsl_raw

adsl <- df_explicit_na(adsl) %>% filter(SAFFL == "Y")

adsl$DTHCAT <- factor(adsl$DTHCAT, levels = c("ADVERSE EVENT", "PROGRESSIVE DISEASE", "OTHER", "<Missing>"))

testthat::test_that("DTHT01 variant 1 is produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    count_values(
      "DTHFL",
      values = "Y",
      .labels = c(count_fraction = "Total number of deaths"),
      .formats = c(count_fraction = "xx (xx.x%)")
    ) %>%
    analyze_vars(vars = c("DTHCAT"), var_labels = c("Primary Cause of Death"))

  result <- build_table(lyt, df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("DTHT01 variant 2 is produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    count_values(
      "DTHFL",
      values = "Y",
      .labels = c(count_fraction = "Total number of deaths"),
      .formats = c(count_fraction = "xx (xx.x%)")
    ) %>%
    analyze_vars(vars = c("DTHCAT"), var_labels = c("Primary Cause of Death")) %>%
    split_rows_by("DTHCAT", split_fun = keep_split_levels("OTHER"), child_labels = "hidden") %>%
    analyze_vars(
      "DTHCAUS",
      .stats = "count_fraction",
      .indent_mods = c("count_fraction" = 2L),
      show_labels = "hidden"
    ) %>%
    analyze_vars(
      vars = "LDDTHGR1",
      nested = FALSE,
      var_labels = "Days from last drug administration",
      show_labels = "visible"
    ) %>%
    split_rows_by(
      "LDDTHGR1",
      split_fun = remove_split_levels("<Missing>"),
      split_label = "Primary cause by days from last study drug administration",
      label_pos = "visible"
    ) %>%
    analyze_vars("DTHCAT")

  result <- build_table(lyt, df = adsl) %>% prune_table()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("DTHT01 variant 3 is produced correctly", {
  dthcaus_levels <- levels(adsl[adsl$DTHCAT == "OTHER", ]$DTHCAUS)

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    count_values(
      "DTHFL",
      values = "Y",
      .labels = c(count_fraction = "Total number of deaths"),
      .formats = c(count_fraction = "xx (xx.x%)")
    ) %>%
    analyze_vars(
      vars = c("DTHCAT"),
      var_labels = c("Primary Cause of Death"),
      table_names = "primary_cause"
    ) %>%
    split_rows_by(
      "DTHCAT",
      split_fun = keep_split_levels("OTHER"),
      child_labels = "hidden"
    ) %>%
    count_values(
      "DTHCAUS",
      values = dthcaus_levels[5],
      .labels = c(count_fraction = "Post-study reportings of death"),
      .formats = c(count_fraction = "xx (xx.x%)"),
      .indent_mods = c(count_fraction = 2L),
      table_names = "post_study_deaths"
    ) %>%
    count_values(
      "DTHCAUS",
      values = dthcaus_levels[-5],
      .labels = c(count_fraction = "All other causes"),
      .formats = c(count_fraction = "xx (xx.x%)"),
      .indent_mods = c(count_fraction = 2L),
      table_names = "all_other_causes"
    )

  result <- build_table(lyt, df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("DTHT01 variant 4 is produced correctly", {
  dthcaus_levels <- levels(adsl[adsl$DTHCAT == "OTHER", ]$DTHCAUS)

  # create a helper variable DTHCAUS_other
  adsl <- adsl %>%
    mutate(
      DTHCAUS_other = factor(ifelse(
        DTHCAT == "OTHER" & DTHCAUS != "Post-study reporting of death", as.character(DTHCAUS), NA
      ), levels = c("LOST TO FOLLOW UP", "SUICIDE", "UNKNOWN", "MISSING")) %>% explicit_na(label = "<Missing>")
    )

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    count_values(
      "DTHFL",
      values = "Y",
      .labels = c(count_fraction = "Total number of deaths"),
      .formats = c(count_fraction = "xx (xx.x%)")
    ) %>%
    analyze_vars(
      vars = c("DTHCAT"),
      var_labels = c("Primary Cause of Death"),
      table_names = "primary_cause"
    ) %>%
    split_rows_by("DTHCAT", split_fun = keep_split_levels("OTHER"), child_labels = "hidden") %>%
    count_values(
      "DTHCAUS",
      values = dthcaus_levels[5],
      .labels = c(count_fraction = "Post-study reportings of death"),
      .formats = c(count_fraction = "xx (xx.x%)"),
      .indent_mods = c(count_fraction = 2L),
      table_names = "post_study_deaths"
    ) %>%
    count_values(
      "DTHCAUS",
      values = dthcaus_levels[-5],
      .labels = c(count_fraction = "All other causes"),
      .formats = c(count_fraction = "xx (xx.x%)"),
      .indent_mods = c(count_fraction = 2L),
      table_names = "all_other_causes"
    ) %>%
    analyze_vars(
      "DTHCAUS_other",
      .stats = "count_fraction",
      .indent_mods = c("count_fraction" = 3L),
      show_labels = "hidden"
    )

  result <- build_table(lyt, df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
