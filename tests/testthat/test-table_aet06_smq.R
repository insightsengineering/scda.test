# Test variants for AET06_SMQ.
adsl <- adsl_pharmaverse
adae <- adae_pharmaverse

testthat::test_that("AET06_SMQ variant 1 is produced correctly", {
  adsl_labels <- var_labels(adsl)
  adae_labels <- var_labels(adae)

  adae <- adae %>%
    dplyr::mutate(
      SMQ1 = dplyr::case_when(
        AEDECOD %in% c("NAUSEA", "VOMITING") ~ "SMQ 1 (broad)",
        TRUE ~ NA_character_
      ),
      SMQ2 = dplyr::case_when(
        AEDECOD %in% c("VOMITING") ~ "SMQ 1 (narrow)",
        TRUE ~ NA_character_
      ),
      SMQ3 = dplyr::case_when(
        AEDECOD %in% c("HEADACHE") ~ "AESI",
        TRUE ~ NA_character_
      )
    )

  adae_smq1 <- adae %>%
    dplyr::filter(!is.na(SMQ1)) %>%
    dplyr::rename(SMQ = SMQ1) %>%
    dplyr::select(-SMQ2, -SMQ3)

  adae_smq2 <- adae %>%
    dplyr::filter(!is.na(SMQ2)) %>%
    dplyr::rename(SMQ = SMQ2) %>%
    dplyr::select(-SMQ1, -SMQ3)

  adae_smq3 <- adae %>%
    dplyr::filter(!is.na(SMQ3)) %>%
    dplyr::rename(SMQ = SMQ3) %>%
    dplyr::select(-SMQ1, -SMQ2)

  adae_f <- rbind(adae_smq1, adae_smq2, adae_smq3)

  var_labels(adae_f) <- c(adae_labels, "SMQ" = "Standardised MedDRA Queries")

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_cols_by("SEX") %>%
    add_colcounts() %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique"),
      .labels = c(unique = "Total number of patients with at least one adverse event")
    ) %>%
    split_rows_by(
      "SMQ",
      child_labels = "visible",
      nested = FALSE,
      split_fun = drop_split_levels
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Total number of events"
      )
    ) %>%
    count_occurrences(vars = "AEDECOD")

  result <- build_table(lyt, adae_f, alt_counts_df = adsl) %>%
    sort_at_path(path = c("SMQ", "*", "AEDECOD"), scorefun = score_occurrences)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET06_SMQ variant 2 is produced correctly", {
  adsl_labels <- var_labels(adsl)
  adae_labels <- var_labels(adae)

  adsl <- adsl %>%
    dplyr::mutate(
      AGE65 = dplyr::case_when(
        AGE >= 65 ~ ">= 65",
        TRUE ~ "< 65"
      ),
      AGE65 = factor(AGE65, levels = c(">= 65", "< 65"))
    )

  var_labels(adsl) <- c(adsl_labels, "AGE65" = "AGE65 GROUP")

  adae <- adae %>%
    dplyr::mutate(
      SMQ1 = dplyr::case_when(
        AEDECOD %in% c("NAUSEA", "VOMITING") ~ "SMQ 1 (broad)",
        TRUE ~ NA_character_
      ),
      SMQ2 = dplyr::case_when(
        AEDECOD %in% c("VOMITING") ~ "SMQ 1 (narrow)",
        TRUE ~ NA_character_
      ),
      SMQ3 = dplyr::case_when(
        AEDECOD %in% c("HEADACHE") ~ "AESI",
        TRUE ~ NA_character_
      ),
      AGE65 = dplyr::case_when(
        AGE >= 65 ~ ">= 65",
        TRUE ~ "< 65"
      ),
      AGE65 = factor(AGE65, levels = c(">= 65", "< 65"))
    )

  adae_smq1 <- adae %>%
    dplyr::filter(!is.na(SMQ1)) %>%
    dplyr::rename(SMQ = SMQ1) %>%
    dplyr::select(-SMQ2, -SMQ3)

  adae_smq2 <- adae %>%
    dplyr::filter(!is.na(SMQ2)) %>%
    dplyr::rename(SMQ = SMQ2) %>%
    dplyr::select(-SMQ1, -SMQ3)

  adae_smq3 <- adae %>%
    dplyr::filter(!is.na(SMQ3)) %>%
    dplyr::rename(SMQ = SMQ3) %>%
    dplyr::select(-SMQ1, -SMQ2)

  adae_f <- rbind(adae_smq1, adae_smq2, adae_smq3)

  var_labels(adae_f) <- c(adae_labels, "SMQ" = "Standardised MedDRA Queries", "AGE65" = "AGE65 GROUP")

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_cols_by("AGE65") %>%
    add_colcounts() %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique"),
      .labels = c(unique = "Total number of patients with at least one adverse event")
    ) %>%
    split_rows_by(
      "SMQ",
      child_labels = "visible",
      nested = FALSE,
      split_fun = drop_split_levels
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Total number of events"
      )
    ) %>%
    count_occurrences(vars = "AEDECOD")

  result <- build_table(lyt, adae_f, alt_counts_df = adsl) %>%
    sort_at_path(path = c("SMQ", "*", "AEDECOD"), scorefun = score_occurrences)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
