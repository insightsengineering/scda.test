# Tests all variants of EXT01

adsl <- adsl_pharmaverse
adex <- adex_pharmaverse

adsl <- df_explicit_na(adsl)
adex <- df_explicit_na(adex)

adex <- adex %>%
  filter(PARCAT1 == "OVERALL") %>%
  select(STUDYID, USUBJID, ACTARM, PARAMCD, PARAM, AVAL, PARCAT2) %>%
  mutate(
    PARAMCD = as.character(PARAMCD),
    AVALC = ""
  ) %>%
  droplevels()

set.seed(99)
tdurd_adsl <- adsl %>%
  select(STUDYID, USUBJID, ACTARM) %>%
  mutate(
    PARAMCD = "TDURD",
    PARAM = "Overall duration (days)",
    AVAL = sample(1:150, size = nrow(adsl), replace = TRUE),
    AVALC = case_when(
      0 <= AVAL & AVAL <= 30 ~ "0 - 30",
      31 <= AVAL & AVAL <= 60 ~ "31 - 60",
      61 <= AVAL & AVAL <= 90 ~ "61 - 90",
      TRUE ~ ">= 91"
    )
  )
tdurd <- adex %>%
  filter(PARAMCD == "TNDOSE") %>%
  select(STUDYID, USUBJID, PARCAT2) %>%
  left_join(tdurd_adsl, by = c("STUDYID", "USUBJID"))

tndosmis_adsl <- adsl %>%
  select(STUDYID, USUBJID, ACTARM) %>%
  mutate(
    PARAMCD = "TNDOSMIS",
    PARAM = "Total number of missed doses during study",
    AVAL = sample(0:20, size = nrow(adsl), replace = TRUE),
    AVALC = ""
  )
tndosmis <- adex %>%
  filter(PARAMCD == "TNDOSE") %>%
  select(STUDYID, USUBJID, PARCAT2) %>%
  left_join(tndosmis_adsl, by = c("STUDYID", "USUBJID"))

adex <- dplyr::bind_rows(adex, tdurd, tndosmis) %>%
  mutate(PARAM = factor(
    PARAM,
    levels = c(
      "Overall duration (days)", "Total dose administered", "Total number of doses administered",
      "Total number of missed doses during study"
    )
  ))

testthat::test_that("EXT01 default variant with numeric parameters is produced correctly", {
  split_fun <- drop_split_levels

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_rows_by("PARCAT2", split_label = "\nParameter Category (Drug A/Drug B)", label_pos = "topleft") %>%
    split_rows_by("PARAM", split_fun = split_fun) %>%
    analyze_vars(vars = "AVAL")

  result <- build_table(lyt = lyt, df = adex, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("EXT01 variant: with both numeric and categorical parameters", {
  adex_avalc_wide <- adex %>%
    filter(PARAMCD == "TDURD") %>%
    select(STUDYID, USUBJID, PARAMCD, AVALC, PARCAT2) %>%
    tidyr::pivot_wider(
      id_cols = c(STUDYID, USUBJID, PARCAT2),
      names_from = PARAMCD,
      values_from = AVALC
    ) %>%
    mutate(
      TDURDC = factor(TDURD, levels = c("0 - 30", "31 - 60", "61 - 90", ">= 91"))
    ) %>%
    select(-TDURD)

  anl <- adex %>%
    select(STUDYID, USUBJID, ACTARM, PARAMCD, AVAL, PARCAT2) %>%
    tidyr::pivot_wider(
      id_cols = c(STUDYID, USUBJID, ACTARM, PARCAT2),
      names_from = PARAMCD,
      values_from = AVAL
    ) %>%
    left_join(adex_avalc_wide, by = c("STUDYID", "USUBJID", "PARCAT2")) %>%
    var_relabel(
      TDOSE = "Total dose administered",
      TNDOSE = "Total number of doses administered",
      TDURD = "Overall duration (days)",
      TNDOSMIS = "Total number of missed doses during study",
      TDURDC = "Overall duration (days)"
    )

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_rows_by("PARCAT2", split_label = "\nParameter Category (Drug A/Drug B)", label_pos = "topleft") %>%
    analyze_vars(
      vars = c("TDURD", "TDURDC", "TDOSE", "TNDOSE"),
      var_labels = var_labels(anl)[c("TDURD", "TDURDC", "TDOSE", "TNDOSE")]
    )

  result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("EXT01 variant: with user specified categories for missed doses", {
  adex_avalc_wide <- adex %>%
    filter(PARAMCD == "TDURD") %>%
    select(STUDYID, USUBJID, PARAMCD, AVALC, PARCAT2) %>%
    tidyr::pivot_wider(
      id_cols = c(STUDYID, USUBJID, PARCAT2),
      names_from = PARAMCD,
      values_from = AVALC
    ) %>%
    mutate(
      TDURDC = factor(TDURD, levels = c("0 - 30", "31 - 60", "61 - 90", ">= 91"))
    ) %>%
    select(-TDURD)

  anl <- adex %>%
    select(STUDYID, USUBJID, ACTARM, PARAMCD, AVAL, PARCAT2) %>%
    tidyr::pivot_wider(
      id_cols = c(STUDYID, USUBJID, ACTARM, PARCAT2),
      names_from = PARAMCD,
      values_from = AVAL
    ) %>%
    left_join(adex_avalc_wide, by = c("STUDYID", "USUBJID", "PARCAT2")) %>%
    var_relabel(
      TDOSE = "Total dose administered",
      TNDOSE = "Total number of doses administered",
      TDURD = "Overall duration (days)",
      TNDOSMIS = "Total number of missed doses during study",
      TDURDC = "Overall duration (days)"
    )

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_rows_by("PARCAT2", split_label = "\nParameter Category (Drug A/Drug B)", label_pos = "topleft") %>%
    analyze_vars(
      vars = c("TDURD", "TDURDC", "TDOSE", "TNDOSE"),
      var_labels = var_labels(anl)[c("TDURD", "TDURDC", "TDOSE", "TNDOSE")]
    ) %>%
    count_missed_doses(
      "TNDOSMIS",
      thresholds = c(1, 5, 10, 15),
      var_labels = "Missed Doses"
    )

  result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
