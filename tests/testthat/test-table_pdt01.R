adsl <- adsl_raw
addv <- addv_raw

adsl <- df_explicit_na(adsl)
addv <- df_explicit_na(addv)

addv <- addv %>%
  var_relabel(
    DVDECOD = "Category",
    DVTERM = "Description"
  )

testthat::test_that("PDT01 is produced correctly", {
  split_fun <- drop_split_levels

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one major protocol deviation",
        nonunique = "Total number of major protocol deviations"
      )
    ) %>%
    split_rows_by(
      "DVDECOD",
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(addv$DVDECOD)
    ) %>%
    count_occurrences(vars = "DVTERM") %>%
    append_varlabels(addv, "DVTERM", indent = 1L)

  result <- build_table(lyt = lyt, df = addv, alt_counts_df = adsl) %>%
    prune_table() %>%
    sort_at_path(path = c("DVDECOD", "*", "DVTERM"), scorefun = score_occurrences)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
