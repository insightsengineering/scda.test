adsl <- adsl_raw
adae <- adae_raw

# Ensure character variables are converted to factors and empty strings and NAs are explicit missing levels.
adsl <- df_explicit_na(adsl)
adae <- df_explicit_na(adae)

adae_nonser <- adae %>% filter(AESER != "Y", SAFFL == "Y")

get_adae_trimmed <- function(adsl, adae, cutoff_rate) {
  n_per_arm <- adsl %>%
    dplyr::count(ARM)

  anl_terms <- adae %>%
    dplyr::group_by(ARM, AEBODSYS, AEDECOD) %>%
    dplyr::count(
      unique_terms = n_distinct(USUBJID)
    ) %>%
    dplyr::select(-n) %>%
    dplyr::ungroup()

  anl_terms <- dplyr::left_join(
    anl_terms,
    n_per_arm,
    by = "ARM",
    multiple = "all"
  ) %>%
    dplyr::mutate(
      ae_rate = unique_terms / n
    ) %>%
    dplyr::filter(ae_rate >= cutoff_rate) %>%
    dplyr::select(AEDECOD) %>%
    unique()

  anl <- dplyr::left_join(
    anl_terms,
    adae,
    by = "AEDECOD",
    multiple = "all"
  )
  anl
}

adae_trim <- get_adae_trimmed(adsl, adae_nonser, cutoff_rate = 0.05)

testthat::test_that("EUDRAT01 is produced correctly", {
  split_fun <- drop_split_levels

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    summarize_patients_events_in_cols(
      custom_label = paste(
        "Total number of patients with at least one non-serious adverse event",
        "occuring at a relative frequency of >=5% and number of events"
      )
    ) %>%
    split_rows_by("AEBODSYS",
      nested = FALSE,
      split_fun = split_fun,
      indent_mod = -1L,
      label_pos = "topleft",
      split_label = obj_label(adae_trim$AEBODSYS)
    ) %>%
    split_rows_by(
      "AEDECOD",
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adae_trim$AEDECOD)
    ) %>%
    summarize_patients_events_in_cols(
      col_split = FALSE
    )

  result <- build_table(lyt, adae_trim)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
