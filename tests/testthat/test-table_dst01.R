adsl <- adsl_raw

adsl <- df_explicit_na(adsl) %>%
  mutate(EOSSTT = factor(EOSSTT, levels = c("COMPLETED", "ONGOING", "DISCONTINUED")))

adsl_gp_added <- adsl %>%
  mutate(DCSREASGP = case_when(
    DCSREAS %in% c("ADVERSE EVENT", "DEATH") ~ "Safety",
    (DCSREAS != "<Missing>" & !DCSREAS %in% c("ADVERSE EVENT", "DEATH")) ~ "Non-Safety",
    DCSREAS == "<Missing>" ~ "<Missing>"
  ) %>% factor(levels = c("Safety", "Non-Safety", "<Missing>")))

adsl_eotstt_added <- adsl_gp_added %>%
  mutate(
    EOTSTT = sample(
      c("ONGOING", "COMPLETED", "DISCONTINUED"),
      size = nrow(adsl),
      replace = TRUE
    ) %>% factor(levels = c("COMPLETED", "ONGOING", "DISCONTINUED"))
  )

testthat::test_that("DST01 default variant is produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(
      "ACTARM",
      split_fun = add_overall_level("All Patients", first = FALSE)
    ) %>%
    count_occurrences(
      "EOSSTT",
      .stats = "count_fraction",
      show_labels = "hidden"
    ) %>%
    summarize_vars(
      "DCSREAS",
      .stats = "count_fraction",
      denom = "N_col",
      show_labels = "hidden",
      .indent_mods = c(count_fraction = 1L)
    )

  result1 <- build_table(lyt = lyt, df = adsl)

  res <- testthat::expect_silent(result1)
  testthat::expect_snapshot(res)
})

testthat::test_that("DST01 variants 2 and 3 are produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(
      "ACTARM",
      split_fun = add_overall_level("All Patients", first = FALSE)
    ) %>%
    count_occurrences(
      "EOSSTT",
      .stats = "count_fraction",
      show_labels = "hidden"
    ) %>%
    split_rows_by("DCSREASGP", indent_mod = 1L) %>%
    summarize_vars(
      "DCSREAS",
      .stats = "count_fraction",
      denom = "N_col",
      show_labels = "hidden"
    )

  tbl <- build_table(lyt = lyt, df = adsl_gp_added)
  result2 <- prune_table(tbl) # remove rows containing all zeros

  res <- testthat::expect_silent(result2)
  testthat::expect_snapshot(res)

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(
      "ACTARM",
      split_fun = add_overall_level("All Patients", first = FALSE)
    ) %>%
    count_occurrences(
      "EOTSTT",
      .stats = "count_fraction",
      show_labels = "hidden"
    )

  tbl <- build_table(lyt = lyt, df = adsl_eotstt_added)
  tbl <- prune_table(tbl) # remove rows containing all zeros

  # Combine tables
  col_info(result2) <- col_info(tbl)
  result3 <- rbind(result2, tbl)

  res <- testthat::expect_silent(result3)
  testthat::expect_snapshot(res)
})
