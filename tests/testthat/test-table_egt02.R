# Tests the variants for EGT02

adsl <- adsl_raw
adeg <- adeg_raw

adsl <- df_explicit_na(adsl)
adeg <- df_explicit_na(adeg)

adeg_f <- adeg %>%
  filter(ONTRTFL == "Y") %>%
  filter(PARAM %in% c("Heart Rate", "QT Duration", "RR Duration")) %>%
  filter(ANRIND != "<Missing>") %>%
  var_relabel(
    PARAM = "Assessment",
    ANRIND = "Abnormality"
  )

testthat::test_that("(EGT02) 1. Regardless of Abnormality at Baseline", {
  split_fun <- drop_split_levels

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ACTARM") %>%
    split_rows_by(
      "PARAM",
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adeg_f$PARAM)
    ) %>%
    count_abnormal("ANRIND", abnormal = list(Low = "LOW", High = "HIGH"), exclude_base_abn = FALSE) %>%
    append_varlabels(adeg_f, "ANRIND", indent = 1L)

  result <- build_table(lyt = lyt, df = adeg_f, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("(EGT02) 2. Among Subjects Without Abnormality at Baseline", {
  split_fun <- drop_split_levels

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ACTARM") %>%
    split_rows_by(
      "PARAM",
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adeg_f$PARAM)
    ) %>%
    count_abnormal("ANRIND", abnormal = list(Low = "LOW", High = "HIGH"), exclude_base_abn = TRUE) %>%
    append_varlabels(adeg_f, "ANRIND", indent = 1L)

  result <- build_table(lyt = lyt, df = adeg_f, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
