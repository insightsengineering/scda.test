# 1. Preprocess ADAE so that deaths do not occur in arm "A: Drug X".
# 2. Concatenate AEBODSYS and AEDECOD per GDSR output standard AET07.
preprocess_adae <- function(adae) {
  set.seed(1, kind = "Mersenne-Twister")
  adae %>%
    dplyr::mutate(
      # Convert AESDTH to character for next step.
      AESDTH = as.character(AESDTH),
      # For demonstration purpose only,
      # make "A: Drug X" as the arm without AE leading to death.
      AESDTH = dplyr::case_when(
        ARM == "A: Drug X" ~ NA_character_,
        TRUE ~ AESDTH
      ),
      AESDTH = as.factor(AESDTH),
      SOC_PT = factor(paste(AEBODSYS, "/", AEDECOD))
    ) %>%
    dplyr::filter(AESDTH == "Y")
}

adsl <- pharmaverseadam::adsl %>%
  mutate(DCSREAS = sample(c("ADVERSE EVENT", ""), nrow(.), replace = TRUE, prob = c(0.08, 0.92)),
         DCSREAS = with_label(DCSREAS, "Discontinuation Reason")) %>%
  filter(ACTARM != "Screen Failure")

adae <- pharmaverseadam::adae %>%
  mutate(AETOXGR = sample(c("1", "2", "3", "4", "5"), nrow(.), replace = TRUE, prob = c(0.70, 0.20, 0.05, 0.045, 0.005)),
         ANL01FL = "Y")

testthat::test_that("AET07 variant 1 is produced correctly", {
  adae <- adae %>%
    preprocess_adae()

  lyt <- basic_table() %>%
    split_cols_by("ACTARM", split_fun = drop_split_levels) %>%
    add_colcounts() %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = "unique",
      .labels = c(unique = "Total number of deaths"),
      show_labels = "hidden"
    ) %>%
    count_occurrences(
      vars = "SOC_PT"
    )

  result <- build_table(lyt, adae, alt_counts_df = adsl) %>%
    sort_at_path(
      path = "SOC_PT",
      scorefun = score_occurrences,
      decreasing = TRUE
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET07 variant 2 is produced correctly", {
  adae <- adae %>%
    preprocess_adae()

  lyt <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = "unique",
      .labels = c(unique = "Total number of deaths"),
      show_labels = "hidden"
    ) %>%
    count_occurrences(
      vars = "SOC_PT"
    )

  result <- build_table(lyt, adae, alt_counts_df = adsl) %>%
    prune_table() %>%
    sort_at_path(
      path = "SOC_PT",
      scorefun = score_occurrences,
      decreasing = TRUE
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
