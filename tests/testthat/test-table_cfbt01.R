adsl <- adsl_raw
adqs <- adqs_raw

adsl <- df_explicit_na(adsl)
adqs <- df_explicit_na(adqs)

adqs <- adqs %>%
  dplyr::filter(
    PARAM == "BFI All Questions",
    AVISIT != "SCREENING"
  ) %>%
  var_relabel(
    AVISIT = "Visit"
  )

split_fun <- drop_split_levels

testthat::test_that("CFBT01 default variant is produced correctly", {
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by(
      "AVISIT",
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adqs$AVISIT)
    ) %>%
    split_cols_by_multivar(
      vars = c("AVAL", "CHG"),
      varlabels = c("Value at Visit", "Change from\nBaseline")
    ) %>%
    summarize_colvars()

  result <- build_table(lyt, adqs)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
