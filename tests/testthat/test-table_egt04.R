# Tests the single variant for EGT04
set.seed(99)

adsl <- adsl_pharmaverse
adeg <- adeg_pharmaverse %>%
  group_by(USUBJID, AVISIT, PARAMCD) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(
    AVALU = EGSTRESU,
    WORS02FL = sample(c("Y", ""), nrow(.), replace = TRUE, prob = c(0.25, 0.75)),
    BASEC = sample(c("NORMAL", "ABNORMAL", "Missing"), nrow(.), replace = TRUE, prob = c(0.5, 0.3, 0.2))
  )

testthat::test_that("EGT04 default variant is produced correctly", {
  adeg_labels <- var_labels(adeg)
  adeg_f <- subset(
    adeg,
    PARAMCD == "EGINTP" & # Analysis in terms of "NORMAL"/"ABNORMAL" (AVALC)
      SAFFL == "Y" & # "Safety Population Flag"
      ONTRTFL == "Y" & # "On Treatment Record Flag"
      WORS02FL == "Y" # "Worst Post-Baseline Observation"
  )

  # Preprocessing
  # For the EGT04 template, data imputation shoud be avoided, and missing data
  # explicit and accounted for, so the contingency table sum adds up to the group N.
  # For illustration purpose, missing data are added to the example.
  set.seed(123, kind = "Mersenne-Twister")

  adeg_f$AVALC[sample(seq_len(nrow(adeg_f)), size = 5)] <- "Missing"
  adeg_f$BASEC[sample(seq_len(nrow(adeg_f)), size = 5)] <- "Missing"

  adeg_f$AVALC <- factor( # nolint
    adeg_f$AVALC,
    levels = c("NORMAL", "ABNORMAL", "Missing"),
    labels = c("Normal", "Abnormal", "Missing")
  )
  adeg_f$BASEC <- factor( # nolint
    adeg_f$BASEC,
    levels = c("NORMAL", "ABNORMAL", "Missing"),
    labels = c("Normal", "Abnormal", "Missing")
  )
  var_labels(adeg_f) <- adeg_labels

  l <- basic_table() %>%
    split_cols_by("AVALC") %>%
    split_rows_by("ARM") %>%
    add_rowcounts() %>%
    analyze_vars(
      "BASEC",
      denom = "N_row",
      .stats = "count_fraction",
      na.rm = FALSE
    )

  result <- build_table(l, adeg_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
