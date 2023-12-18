# Tests the single variant of AET10

adsl <- pharmaverseadam::adsl %>%
  mutate(DCSREAS = sample(c("ADVERSE EVENT", ""), nrow(.), replace = TRUE, prob = c(0.08, 0.92)),
         DCSREAS = with_label(DCSREAS, "Discontinuation Reason")) %>%
  filter(ACTARM != "Screen Failure")

adae <- pharmaverseadam::adae %>%
  mutate(AETOXGR = sample(c("1", "2", "3", "4", "5"), nrow(.), replace = TRUE, prob = c(0.70, 0.20, 0.05, 0.045, 0.005)),
         ANL01FL = "Y")

testthat::test_that("AET10 default variant is produced correctly", {
  result1 <- basic_table() %>%
    split_cols_by(var = "ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    count_occurrences(vars = "AEDECOD") %>%
    build_table(adae, alt_counts_df = adsl)

  result2 <- prune_table(
    result1,
    keep_rows(
      has_fraction_in_any_col(atleast = 0.30, col_names = levels(adsl$ARM))
    )
  )

  result <- sort_at_path(result2, path = c("AEDECOD"), scorefun = score_occurrences)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
