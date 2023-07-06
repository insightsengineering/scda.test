# Tests all variants of LBT04

adsl <- adsl_raw
adlb <- adlb_raw

adsl <- df_explicit_na(adsl)
adlb <- df_explicit_na(adlb)

adlb_f <- adlb %>%
  filter(ONTRTFL == "Y", ANRIND != "<Missing>") %>%
  var_relabel(
    PARAM = "Laboratory Test",
    ANRIND = "Direction of Abnormality"
  )

testthat::test_that("LBT04 default variant is produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_rows_by("PARAM",
      split_fun = drop_split_levels,
      label_pos = "topleft",
      split_label = obj_label(adlb_f$PARAM)
    ) %>%
    count_abnormal(
      var = "ANRIND",
      abnormal = list(Low = c("LOW", "LOW LOW"), High = c("HIGH", "HIGH HIGH")),
      exclude_base_abn = TRUE
    ) %>%
    append_varlabels(adlb_f, "ANRIND", indent = 1L)

  result <- build_table(lyt = lyt, df = adlb_f, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
