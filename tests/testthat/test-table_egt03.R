# Tests variant 1 for EGT03

adsl <- adsl_pharmaverse %>%
  mutate(ANRIND = NA)
adeg <- adeg_pharmaverse

set.seed(123, kind = "Mersenne-Twister")
adeg_labels <- var_labels(adeg)

# Filtering
# ---------
adeg_f <- subset(
  adeg,
  PARAMCD == "HR" & # Heart Rate
    SAFFL == "Y" & # "Safety Population Flag"
    ONTRTFL == "Y" & # "On Treatment Record Flag"
    AVISIT == "Week 2" & # "Analysis Visit"
    DTYPE == "AVERAGE"
)

# Preprocessing

# For the EGT03 template, data imputation should be avoided, and missing data
# explicit and accounted for, so the contingency table sum adds up to the group N.
# For illustration purpose, missing data are added to the example.
adeg_f$BNRIND[sample(seq_len(nrow(adeg_f)), size = 100)] <- "LOW"
adeg_f$ANRIND[sample(seq_len(nrow(adeg_f)), size = 100)] <- "LOW"

adeg_f$BNRIND <- factor( # nolint
  adeg_f$BNRIND,
  levels = c("LOW", "NORMAL", "HIGH", "Missing"),
  labels = c("LOW", "NORMAL", "HIGH", "Missing")
)

adeg_f$BNRIND[sample(seq_len(nrow(adeg_f)), size = 5)] <- "Missing"

testthat::test_that("EGT03 variant 1 is produced correctly", {
  set.seed(123, kind = "Mersenne-Twister")

  # Preprocessing
  adeg_f$ANRIND <- factor( # nolint
    adeg_f$ANRIND,
    levels = c("LOW", "NORMAL", "HIGH", "Missing"),
    labels = c("LOW", "NORMAL", "HIGH", "Missing")
  )

  adeg_f$ANRIND[sample(seq_len(nrow(adeg_f)), size = 5)] <- "Missing"
  var_labels(adeg_f) <- adeg_labels

  lyt <- basic_table() %>%
    split_cols_by("ANRIND") %>%
    split_rows_by("ARM") %>%
    add_rowcounts(alt_counts = TRUE) %>%
    analyze_vars("BNRIND", denom = "N_row")

  result <- build_table(lyt = lyt, df = adeg_f, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("EGT03 variant 2 is produced correctly", {
  var_labels(adeg_f) <- adeg_labels

  lyt <- basic_table() %>%
    split_cols_by("ANRIND") %>%
    split_rows_by("ARM") %>%
    add_rowcounts(alt_counts = TRUE) %>%
    analyze_vars("BNRIND", denom = "N_row")

  result <- build_table(lyt = lyt, df = adeg_f, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("EGT03 variant 3 is produced correctly", {
  set.seed(123, kind = "Mersenne-Twister")

  # Preprocessing
  adeg_f$ANRIND <- factor( # nolint
    adeg_f$ANRIND,
    levels = c("LOW", "NORMAL", "HIGH", "Missing"),
    labels = c("LOW", "NORMAL", "HIGH", "Missing")
  )

  adeg_f$ANRIND[sample(seq_len(nrow(adeg_f)), size = 5)] <- "Missing"

  var_labels(adeg_f) <- adeg_labels

  lyt <- basic_table() %>%
    split_cols_by("ANRIND") %>%
    split_rows_by("ARM") %>%
    add_rowcounts(alt_counts = TRUE) %>%
    analyze_vars("BNRIND", denom = "N_row")

  result <- build_table(lyt = lyt, df = adeg_f, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("EGT03 variant 4 is produced correctly", {
  set.seed(123, kind = "Mersenne-Twister")

  # Preprocessing
  adeg_f$ANRIND <- factor( # nolint
    adeg_f$ANRIND,
    levels = c("LOW", "NORMAL", "HIGH", "Missing"),
    labels = c("LOW", "NORMAL", "HIGH", "Missing")
  )

  adeg_f$ANRIND[sample(seq_len(nrow(adeg_f)), size = 5)] <- "Missing"
  var_labels(adeg_f) <- adeg_labels

  lyt <- basic_table() %>%
    split_cols_by("ANRIND") %>%
    split_rows_by("ARM") %>%
    add_rowcounts(alt_counts = TRUE) %>%
    analyze_vars("BNRIND", denom = "N_row")

  result <- build_table(lyt = lyt, df = adeg_f, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
