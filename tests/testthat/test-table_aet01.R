adsl <- adsl_raw
adae <- adae_raw

adsl <- df_explicit_na(adsl)
adae <- df_explicit_na(
  adae,
  omit_columns = c("SMQ01NAM", "SMQ01SC", "SMQ02NAM", "SMQ02SC", "CQ01NAM", "STUDYID", "USUBJID")
)

set.seed(99)

adae <- adae %>%
  mutate(
    AEDECOD = with_label(as.character(AEDECOD), "Dictionary-Derived Term"),
    AESDTH = with_label(
      sample(c("N", "Y"), size = nrow(adae), replace = TRUE, prob = c(0.99, 0.01)), "Results in Death"
    ),
    AEACN = with_label(sample(
      c("DOSE NOT CHANGED", "DOSE INCREASED", "DRUG INTERRUPTED", "DRUG WITHDRAWN"),
      size = nrow(adae),
      replace = TRUE, prob = c(0.68, 0.02, 0.25, 0.05)
    ), "Action Taken with Study Treatment"),
    FATAL = with_label(AESDTH == "Y", "AE with fatal outcome"),
    SEV = with_label(AESEV == "SEVERE", "Severe AE (at greatest intensity)"),
    SER = with_label(AESER == "Y", "Serious AE"),
    SERWD = with_label(AESER == "Y" & AEACN == "DRUG WITHDRAWN", "Serious AE leading to withdrawal from treatment"),
    SERDSM = with_label(
      AESER == "Y" & AEACN %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
      "Serious AE leading to dose modification/interruption"
    ),
    RELSER = with_label(AESER == "Y" & AEREL == "Y", "Related Serious AE"),
    WD = with_label(AEACN == "DRUG WITHDRAWN", "AE leading to withdrawal from treatment"),
    DSM = with_label(
      AEACN %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"), "AE leading to dose modification/interruption"
    ),
    REL = with_label(AEREL == "Y", "Related AE"),
    RELWD = with_label(AEREL == "Y" & AEACN == "DRUG WITHDRAWN", "Related AE leading to withdrawal from treatment"),
    RELDSM = with_label(
      AEREL == "Y" & AEACN %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
      "Related AE leading to dose modification/interruption"
    ),
    CTC35 = with_label(AETOXGR %in% c("3", "4", "5"), "Grade 3-5 AE"),
    CTC45 = with_label(AETOXGR %in% c("4", "5"), "Grade 4/5 AE"),
    SMQ01 = with_label(SMQ01NAM != "", aesi_label(adae$SMQ01NAM, adae$SMQ01SC)),
    SMQ02 = with_label(SMQ02NAM != "", aesi_label(adae$SMQ02NAM, adae$SMQ02SC)),
    CQ01 = with_label(CQ01NAM != "", aesi_label(adae$CQ01NAM)),
    USUBJID_AESEQ = paste(USUBJID, AESEQ, sep = "@@") # Create unique ID per AE in dataset.
  ) %>%
  filter(ANL01FL == "Y")

testthat::test_that("Safety Summary Variant 1 works as expected", {
  aesi_vars <- c("FATAL", "SER", "SERWD", "SERDSM", "RELSER", "WD", "DSM", "REL", "RELWD", "RELDSM", "SEV")

  # Layout for variables from adsl dataset.
  lyt_adsl <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    count_patients_with_event(
      "USUBJID",
      filters = c("DTHFL" = "Y"),
      denom = "N_col",
      .labels = c(count_fraction = "Total number of deaths")
    ) %>%
    count_patients_with_event(
      "USUBJID",
      filters = c("DCSREAS" = "ADVERSE EVENT"),
      denom = "N_col",
      .labels = c(count_fraction = "Total number of patients withdrawn from study due to an AE"),
      table_names = "tot_wd"
    )

  result_adsl <- build_table(lyt_adsl, df = adsl, alt_counts_df = adsl)

  # Layout for variables from adae dataset.
  lyt_adae <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one AE",
        nonunique = "Total number of AEs"
      ),
      .formats = list(unique = format_count_fraction_fixed_dp, nonunique = "xx"),
      show_labels = "hidden"
    ) %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = var_labels(adae[, aesi_vars]),
      denom = "N_col",
      var_labels = "Total number of patients with at least one",
      show_labels = "visible"
    )

  result_adae <- build_table(lyt_adae, df = adae, alt_counts_df = adsl)

  # Combine tables.
  col_info(result_adsl) <- col_info(result_adae)
  result <- rbind(
    result_adae[1:2, ],
    result_adsl,
    result_adae[3:nrow(result_adae), ]
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("Safety Summary Variant 2 (with Medical Concepts Section) works as expected", {
  aesi_vars <- c("FATAL", "SER", "SERWD", "SERDSM", "RELSER", "WD", "DSM", "REL", "RELWD", "RELDSM", "CTC35")
  basket_vars <- c("SMQ01", "SMQ02", "CQ01")

  # Layout for variables from adsl dataset.
  lyt_adsl <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    count_patients_with_event(
      "USUBJID",
      filters = c("DTHFL" = "Y"),
      denom = "N_col",
      .labels = c(count_fraction = "Total number of deaths")
    ) %>%
    count_patients_with_event(
      "USUBJID",
      filters = c("DCSREAS" = "ADVERSE EVENT"),
      denom = "N_col",
      .labels = c(count_fraction = "Total number of patients withdrawn from study due to an AE"),
      table_names = "tot_wd"
    )

  result_adsl <- build_table(lyt_adsl, df = adsl, alt_counts_df = adsl)

  # Layout for variables from adae dataset.
  lyt_adae <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one AE",
        nonunique = "Total number of AEs"
      ),
      .formats = list(unique = format_count_fraction_fixed_dp, nonunique = "xx"),
      show_labels = "hidden"
    ) %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = var_labels(adae[, aesi_vars]),
      denom = "N_col",
      var_labels = "Total number of patients with at least one",
      show_labels = "visible"
    ) %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = var_labels(adae[, basket_vars]),
      table_names = "table_aesi",
      denom = "N_col",
      var_labels = "Total number of patients with at least one",
      show_labels = "visible"
    )

  result_adae <- build_table(lyt_adae, df = adae, alt_counts_df = adsl)

  # Combine tables.
  col_info(result_adsl) <- col_info(result_adae)
  result <- rbind(
    result_adae[1:2, ],
    result_adsl,
    result_adae[3:nrow(result_adae), ]
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("Safety Summary Variant 3 (with Modified Rows) works as expected", {
  aesi_vars <- c("FATAL", "SER", "WD", "REL", "CTC35", "CTC45")
  # Layout for variables from adsl dataset.
  lyt_adsl <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    count_patients_with_event(
      "USUBJID",
      filters = c("DTHFL" = "Y"),
      denom = "N_col",
      .labels = c(count_fraction = "Total number of deaths")
    ) %>%
    count_patients_with_event(
      "USUBJID",
      filters = c("DCSREAS" = "ADVERSE EVENT"),
      denom = "N_col",
      .labels = c(count_fraction = "Total number of patients withdrawn from study due to an AE"),
      table_names = "tot_wd"
    ) %>%
    count_patients_with_event(
      "USUBJID",
      filters = c("DCSREAS" = "WITHDRAWAL BY SUBJECT"),
      denom = "N_col",
      .labels = c(count_fraction = "Total number of patients withdrawn informed consent"),
      table_names = "tot_dscsreas_wd"
    )
  result_adsl <- build_table(lyt_adsl, df = adsl, alt_counts_df = adsl)

  # Layout for variables from adae dataset.
  lyt_adae <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one AE",
        nonunique = "Total number of AEs"
      ),
      .formats = list(unique = format_count_fraction_fixed_dp, nonunique = "xx"),
      show_labels = "hidden"
    ) %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = var_labels(adae[, aesi_vars]),
      denom = "N_col",
      var_labels = "Total number of patients with at least one",
      show_labels = "visible"
    )
  result_adae <- build_table(lyt_adae, df = adae, alt_counts_df = adsl)

  # Combine tables.
  col_info(result_adsl) <- col_info(result_adae)
  result <- rbind(
    result_adae[1:2, ],
    result_adsl,
    result_adae[3:nrow(result_adae), ]
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("Safety Summary Variant 4 (with Rows Counting Events and Additional Sections) works as expected", {
  count_subj_vars <- c("FATAL", "SER", "WD", "DSM", "REL", "CTC35")
  count_term_vars <- c("SER", "DSM", "REL", "CTC35", "CTC45")
  count_ae_vars <- c("SER", "DSM", "REL", "CTC35", "CTC45")

  # Layout for variables from adsl dataset.
  lyt_adsl <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    count_patients_with_event(
      "USUBJID",
      filters = c("DTHFL" = "Y"),
      denom = "N_col",
      .labels = c(count_fraction = "Total number of deaths")
    ) %>%
    count_patients_with_event(
      "USUBJID",
      filters = c("DCSREAS" = "ADVERSE EVENT"),
      denom = "N_col",
      .labels = c(count_fraction = "Total number of patients withdrawn from study due to an AE"),
      table_names = "tot_wd"
    )

  result_adsl <- build_table(lyt_adsl, df = adsl, alt_counts_df = adsl)

  # Layout for variables from adae dataset.
  lyt_adae <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one AE",
        nonunique = "Total number of AEs"
      ),
      .formats = list(unique = format_count_fraction_fixed_dp, nonunique = "xx"),
      show_labels = "hidden"
    ) %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = var_labels(adae[, count_subj_vars]),
      denom = "N_col",
      var_labels = "Total number of patients with at least one",
      show_labels = "visible"
    ) %>%
    count_patients_with_flags(
      "AEDECOD",
      flag_variables = var_labels(adae[, count_term_vars]),
      .stats = "count",
      .formats = c(count = "xx"),
      table_names = "table_term",
      var_labels = "Total number of unique preferred terms which are",
      show_labels = "visible"
    ) %>%
    count_patients_with_flags(
      "USUBJID_AESEQ",
      flag_variables = var_labels(adae[, count_ae_vars]),
      .stats = "count",
      .formats = c(count = "xx"),
      table_names = "table_ae",
      var_labels = "Total number of adverse events which are",
      show_labels = "visible"
    )

  result_adae <- build_table(lyt_adae, df = adae, alt_counts_df = adsl)

  # Combine tables.
  col_info(result_adsl) <- col_info(result_adae)
  result <- rbind(
    result_adae[1:2, ],
    result_adsl,
    result_adae[3:nrow(result_adae), ]
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
