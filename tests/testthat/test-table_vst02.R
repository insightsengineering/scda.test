# Test the single variant for VST02

adsl <- adsl_raw
advs <- advs_raw

adsl <- df_explicit_na(adsl)
advs <- df_explicit_na(advs)

advs_f <- advs %>%
  filter(ONTRTFL == "Y", ANRIND != "<Missing>") %>%
  var_relabel(
    PARAM = "Assessment",
    ANRIND = "Abnormality"
  )

split_fun <- drop_split_levels

testthat::test_that("1. Vital Sign Abnormalities (Regardless of Abnormality at Baseline, VST02_1)", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ACTARM") %>%
    add_overall_col("All Patients") %>%
    split_rows_by("PARAM", split_fun = split_fun, label_pos = "topleft", split_label = obj_label(advs_f$PARAM)) %>%
    count_abnormal(
      "ANRIND",
      abnormal = list(Low = c("LOW", "LOW LOW"), High = c("HIGH", "HIGH HIGH")),
      exclude_base_abn = FALSE
    ) %>%
    append_varlabels(advs_f, "ANRIND", indent = 1L)

  result <- build_table(lyt = lyt, df = advs_f, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("2. Vital Sign Abnormalities (Among Subject Without Abnormality at Baseline, VST02_2)", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ACTARM") %>%
    add_overall_col("All Patients") %>%
    split_rows_by("PARAM", split_fun = split_fun, label_pos = "topleft", split_label = obj_label(advs_f$PARAM)) %>%
    count_abnormal(
      "ANRIND",
      abnormal = list(Low = c("LOW", "LOW LOW"), High = c("HIGH", "HIGH HIGH")),
      exclude_base_abn = TRUE
    ) %>%
    append_varlabels(advs_f, "ANRIND", indent = 1L)

  result <- build_table(lyt = lyt, df = advs_f, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
