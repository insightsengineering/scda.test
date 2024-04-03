# Test all variants of AET09 SMQ, adverse events related to stuy drug by standardized MEDDRA query
stack_adae_by_smq <- function(adae, smq) {
  l_df <- lapply(smq, function(ae_grp) {
    keep <- !(is.na(adae[[ae_grp]]))
    df <- adae[keep, ]
    df[["SMQ"]] <- ae_grp
    df
  })
  do.call(rbind, l_df)
}

adsl <- adsl_pharmaverse
adae <- adae_pharmaverse

testthat::test_that("AET09 variant 1 (AEs related to study drug by SMQ) is produced correctly", {
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

  adae <- stack_adae_by_smq(adae, c("SMQ1"))
  adae_r <- adae[adae$AREL %in% c("PROBABLE", "REMOTE", "POSSIBLE"), ]

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event related to study drug"
      )
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
        unique = "Total number of patients with at least one adverse event related to study drug",
        nonunique = "Total number of events related to study drug"
      )
    ) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = -1L)

  result <- build_table(lyt, adae_r, alt_counts_df = adsl) %>%
    sort_at_path(path = c("SMQ"), scorefun = cont_n_allcols) %>%
    sort_at_path(path = c("SMQ", "*", "AEDECOD"), scorefun = score_occurrences)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that(
  "AET09 variant 2 (AEs related to study drug by SMQ with customized queries) is produced correctly",
  {
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

    adae <- stack_adae_by_smq(adae, c("SMQ1", "SMQ2", "SMQ3"))
    adae_r <- adae[adae$AREL %in% c("PROBABLE", "REMOTE", "POSSIBLE"), ]

    lyt <- basic_table() %>%
      split_cols_by("ARM") %>%
      add_colcounts() %>%
      analyze_num_patients(
        vars = "USUBJID",
        .stats = c("unique"),
        .labels = c(
          unique = "Total number of patients with at least one adverse event related to study drug"
        )
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
          unique = "Total number of patients with at least one adverse event related to study drug",
          nonunique = "Total number of events related to study drug"
        )
      ) %>%
      count_occurrences(vars = "AEDECOD", .indent_mods = -1L)

    result <- build_table(lyt, adae_r, alt_counts_df = adsl) %>%
      sort_at_path(path = c("SMQ"), scorefun = cont_n_allcols) %>%
      sort_at_path(path = c("SMQ", "*", "AEDECOD"), scorefun = score_occurrences)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)
