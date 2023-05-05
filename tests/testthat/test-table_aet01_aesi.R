adsl <- adsl_raw
adae <- adae_raw

adsl <- filter(adsl, SAFFL == "Y")
adae <- filter(adae, ANL01FL == "Y" & SAFFL == "Y")

adsl <- df_explicit_na(adsl)
adae <- df_explicit_na(adae)

not_resolved <- adae %>%
  filter(!(AEOUT %in% c("RECOVERED/RESOLVED", "FATAL", "RECOVERED/RESOLVED WITH SEQUELAE"))) %>%
  distinct(USUBJID) %>%
  mutate(
    NOT_RESOLVED = "Y"
  )

adae <- adae %>%
  left_join(not_resolved, by = c("USUBJID")) %>%
  mutate(
    ALL_RESOLVED = is.na(NOT_RESOLVED),
    NOT_RESOLVED = !is.na(NOT_RESOLVED)
  ) %>%
  var_relabel(
    ALL_RESOLVED = "Total number of patients with all non-fatal AEs resolved",
    NOT_RESOLVED = "Total number of patients with at least one non-fatal unresolved or ongoing AE"
  )

adae <- adae %>%
  mutate(
    AEDECOD = as.character(AEDECOD),
    WD = AEACN == "DRUG WITHDRAWN",
    DSM = AEACN %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
    CONTRT = AECONTRT == "Y",
    SER = AESER == "Y",
    REL = AEREL == "Y",
    ALL_RESOLVED_WD = WD == TRUE & ALL_RESOLVED == TRUE,
    ALL_RESOLVED_DSM = DSM == TRUE & ALL_RESOLVED == TRUE,
    ALL_RESOLVED_CONTRT = CONTRT == TRUE & ALL_RESOLVED == TRUE,
    NOT_RESOLVED_WD = WD == TRUE & NOT_RESOLVED == TRUE,
    NOT_RESOLVED_DSM = DSM == TRUE & NOT_RESOLVED == TRUE,
    NOT_RESOLVED_CONTRT = CONTRT == TRUE & NOT_RESOLVED == TRUE,
    SERWD = AESER == "Y" & AEACN == "DRUG WITHDRAWN",
    SERCONTRT = AECONTRT == "Y" & AESER == "Y",
    SERDSM = AESER == "Y" & AEACN %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
    RELWD = AEREL == "Y" & AEACN == "DRUG WITHDRAWN",
    RELDSM = AEREL == "Y" & AEACN %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
    RELCONTRT = AECONTRT == "Y" & AEREL == "Y",
    RELSER = AESER == "Y" & AEREL == "Y"
  ) %>%
  var_relabel(
    WD = "Total number of patients with study drug withdrawn due to AE",
    DSM = "Total number of patients with dose modified/interrupted due to AE",
    CONTRT = "Total number of patients with treatment received for AE",
    SER = "Total number of patients with at least one serious AE",
    REL = "Total number of patients with at least one related AE",
    ALL_RESOLVED_WD = "  No. of patients with study drug withdrawn due to resolved AE",
    ALL_RESOLVED_DSM = "  No. of patients with dose modified/interrupted due to resolved AE",
    ALL_RESOLVED_CONTRT = "  No. of patients with treatment received for resolved AE",
    NOT_RESOLVED_WD = "  No. of patients with study drug withdrawn due to unresolved or ongoing AE",
    NOT_RESOLVED_DSM = "  No. of patients with dose modified/interrupted due to unresolved or ongoing AE",
    NOT_RESOLVED_CONTRT = "  No. of patients with treatment received for unresolved or ongoing AE",
    SERWD = "  No. of patients with study drug withdrawn due to serious AE",
    SERDSM = "  No. of patients with dose modified/interrupted due to serious AE",
    SERCONTRT = "  No. of patients with treatment received for serious AE",
    RELWD = "  No. of patients with study drug withdrawn due to related AE",
    RELDSM = "  No. of patients with dose modified/interrupted due to related AE",
    RELCONTRT = "  No. of patients with treatment received for related AE",
    RELSER = "  No. of patients with serious, related AE"
  )

adae <- adae %>%
  mutate(
    AETOXGR = forcats::fct_recode(AETOXGR,
      "Grade 1" = "1",
      "Grade 2" = "2",
      "Grade 3" = "3",
      "Grade 4" = "4",
      "Grade 5 (fatal outcome)" = "5"
    )
  )

testthat::test_that("AET01_AESI Variant 1 works as expected", {
  aesi_vars <- c("WD", "DSM", "CONTRT", "ALL_RESOLVED", "NOT_RESOLVED", "SER", "REL")

  lyt_adae <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    count_patients_with_event(
      vars = "USUBJID",
      filters = c("STUDYID" = as.character(unique(adae$STUDYID))),
      denom = "N_col",
      .labels = c(count_fraction = "Total number of patients with at least one AE")
    ) %>%
    count_values(
      "STUDYID",
      values = as.character(unique(adae$STUDYID)),
      .stats = "count",
      .labels = c(count = "Total number of AEs"),
      table_names = "total_aes"
    ) %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      var_labels = "Total number of patients with at least one AE by worst grade",
      show_labels = "visible"
    ) %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = var_labels(adae[, aesi_vars]),
      denom = "N_col"
    )

  result <- build_table(lyt_adae, df = adae, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET01_AESI Variant 2 works as expected", {
  aesi_vars <- c(
    "WD", "DSM", "CONTRT", "ALL_RESOLVED", "ALL_RESOLVED_WD", "ALL_RESOLVED_DSM", "ALL_RESOLVED_CONTRT",
    "NOT_RESOLVED", "NOT_RESOLVED_WD", "NOT_RESOLVED_DSM", "NOT_RESOLVED_CONTRT", "SER", "SERWD", "SERDSM",
    "SERCONTRT", "REL", "RELWD", "RELDSM", "RELCONTRT", "RELSER"
  )

  lyt_adae <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    count_patients_with_event(
      vars = "USUBJID",
      filters = c("STUDYID" = as.character(unique(adae$STUDYID))),
      denom = "N_col",
      .labels = c(count_fraction = "Total number of patients with at least one AE")
    ) %>%
    count_values(
      "STUDYID",
      values = as.character(unique(adae$STUDYID)),
      .stats = "count",
      .labels = c(count = "Total number of AEs"),
      table_names = "total_aes"
    ) %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      var_labels = "Total number of patients with at least one AE by worst grade",
      show_labels = "visible"
    ) %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = var_labels(adae[, aesi_vars]),
      denom = "N_col"
    )

  result <- build_table(lyt_adae, df = adae, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET01_AESI Variant 3 works as expected", {
  adsl <- filter(adsl_raw, SAFFL == "Y")
  adae_mult <- filter(adae_raw, ANL01FL == "Y" & SAFFL == "Y")

  adsl <- df_explicit_na(adsl)
  adae_mult <- df_explicit_na(adae_mult)

  # for illustration purposes only, create AEREL1, AEREL2, AEACN1, AEACN2 from respective variables
  adae_mult <- adae_mult %>%
    mutate(
      AEREL1 = AEREL,
      AEREL2 = AEREL,
      AEACN1 = AEACN,
      AEACN2 = AEACN
    )

  not_resolved <- adae_mult %>%
    filter(!(AEOUT %in% c("RECOVERED/RESOLVED", "FATAL", "RECOVERED/RESOLVED WITH SEQUELAE"))) %>%
    distinct(USUBJID) %>%
    mutate(
      NOT_RESOLVED = "Y"
    )

  adae_mult <- adae_mult %>%
    left_join(not_resolved, by = c("USUBJID")) %>%
    mutate(
      ALL_RESOLVED = is.na(NOT_RESOLVED),
      NOT_RESOLVED = !is.na(NOT_RESOLVED)
    ) %>%
    var_relabel(
      ALL_RESOLVED = "Total number of patients with all non-fatal AEs resolved",
      NOT_RESOLVED = "Total number of patients with at least one non-fatal unresolved or ongoing AE"
    )

  adae_mult <- adae_mult %>%
    mutate(
      AEDECOD = as.character(AEDECOD),
      WD1 = AEACN1 == "DRUG WITHDRAWN",
      WD2 = AEACN2 == "DRUG WITHDRAWN",
      DSM1 = AEACN1 %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
      DSM2 = AEACN2 %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
      CONTRT = AECONTRT == "Y",
      SER = AESER == "Y",
      REL1 = AEREL1 == "Y",
      REL2 = AEREL2 == "Y",
      ALL_RESOLVED_WD1 = WD1 == TRUE & ALL_RESOLVED == TRUE,
      ALL_RESOLVED_DSM1 = DSM1 == TRUE & ALL_RESOLVED == TRUE,
      ALL_RESOLVED_CONTRT = CONTRT == TRUE & ALL_RESOLVED == TRUE,
      ALL_RESOLVED_WD2 = WD2 == TRUE & ALL_RESOLVED == TRUE,
      ALL_RESOLVED_DSM2 = DSM2 == TRUE & ALL_RESOLVED == TRUE,
      NOT_RESOLVED_WD1 = WD1 == TRUE & NOT_RESOLVED == TRUE,
      NOT_RESOLVED_DSM1 = DSM1 == TRUE & NOT_RESOLVED == TRUE,
      NOT_RESOLVED_CONTRT = CONTRT == TRUE & NOT_RESOLVED == TRUE,
      NOT_RESOLVED_WD2 = WD2 == TRUE & NOT_RESOLVED == TRUE,
      NOT_RESOLVED_DSM2 = DSM2 == TRUE & NOT_RESOLVED == TRUE,
      SERWD1 = AESER == "Y" & AEACN1 == "DRUG WITHDRAWN",
      SERWD2 = AESER == "Y" & AEACN2 == "DRUG WITHDRAWN",
      SERCONTRT = AECONTRT == "Y" & AESER == "Y",
      SERDSM1 = AESER == "Y" & AEACN1 %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
      SERDSM2 = AESER == "Y" & AEACN2 %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
      REL1WD1 = AEREL1 == "Y" & AEACN1 == "DRUG WITHDRAWN",
      REL1WD2 = AEREL1 == "Y" & AEACN2 == "DRUG WITHDRAWN",
      REL2WD1 = AEREL1 == "Y" & AEACN1 == "DRUG WITHDRAWN",
      REL2WD2 = AEREL1 == "Y" & AEACN2 == "DRUG WITHDRAWN",
      REL1DSM1 = AEREL1 == "Y" & AEACN1 %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
      REL2DSM1 = AEREL2 == "Y" & AEACN1 %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
      REL1DSM2 = AEREL1 == "Y" & AEACN2 %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
      REL2DSM2 = AEREL2 == "Y" & AEACN2 %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
      REL1CONTRT = AECONTRT == "Y" & AEREL1 == "Y",
      REL2CONTRT = AECONTRT == "Y" & AEREL2 == "Y",
      REL1SER = AESER == "Y" & AEREL1 == "Y",
      REL2SER = AESER == "Y" & AEREL2 == "Y"
    ) %>%
    var_relabel(
      WD1 = "Total number of patients with study drug 1 withdrawn due to AE",
      WD2 = "Total number of patients with study drug 2 withdrawn due to AE",
      DSM1 = "Total number of patients with dose of study drug 1 modified/interrupted due to AE",
      DSM2 = "Total number of patients with dose of study drug 2 modified/interrupted due to AE",
      CONTRT = "Total number of patients with treatment received for AE",
      SER = "Total number of patients with at least one serious AE",
      REL1 = "Total number of patients with at least one AE related to study drug 1",
      REL2 = "Total number of patients with at least one AE related to study drug 2",
      ALL_RESOLVED_WD1 = "  No. of patients with study drug 1 withdrawn due to resolved AE",
      ALL_RESOLVED_DSM1 = "  No. of patients with dose of study drug 1 modified/interrupted due to resolved AE",
      ALL_RESOLVED_WD2 = "  No. of patients with study drug 2 withdrawn due to resolved AE",
      ALL_RESOLVED_DSM2 = "  No. of patients with dose of study drug 2 modified/interrupted due to resolved AE",
      ALL_RESOLVED_CONTRT = "  No. of patients with treatment received for resolved AE",
      NOT_RESOLVED_WD1 = "  No. of patients with study drug 1 withdrawn due to unresolved or ongoing AE",
      NOT_RESOLVED_DSM1 = "  No. of patients with dose of study drug 1 modified/interrupted due to unresolved or ongoing AE", # nolint
      NOT_RESOLVED_WD2 = "  No. of patients with study drug 2 withdrawn due to unresolved or ongoing AE",
      NOT_RESOLVED_DSM2 = "  No. of patients with dose of study drug 2 modified/interrupted due to unresolved or ongoing AE", # nolint
      NOT_RESOLVED_CONTRT = "  No. of patients with treatment received for unresolved or ongoing AE",
      SERWD1 = "  No. of patients with study drug 1 withdrawn due to serious AE",
      SERWD2 = "  No. of patients with study drug 2 withdrawn due to serious AE",
      SERDSM1 = "  No. of patients with dose of study drug 1 modified/interrupted due to serious AE",
      SERDSM2 = "  No. of patients with dose of study drug 2 modified/interrupted due to serious AE",
      SERCONTRT = "  No. of patients with treatment received for serious AE",
      REL1WD1 = "  No. of patients with study drug 1 withdrawn due to AE related to study drug 1",
      REL1WD2 = "  No. of patients with study drug 1 withdrawn due to AE related to study drug 2",
      REL2WD1 = "  No. of patients with study drug 2 withdrawn due to AE related to study drug 1",
      REL2WD2 = "  No. of patients with study drug 2 withdrawn due to AE related to study drug 2",
      REL1DSM1 = "  No. of patients with dose of study drug 1 modified/interrupted due to AE related to study drug 1",
      REL1DSM2 = "  No. of patients with dose of study drug 1 modified/interrupted due to AE related to study drug 2",
      REL2DSM1 = "  No. of patients with dose of study drug 2 modified/interrupted due to AE related to study drug 1",
      REL2DSM2 = "  No. of patients with dose of study drug 2 modified/interrupted due to AE related to study drug 2",
      REL1CONTRT = "  No. of patients with treatment received for AE related to study drug 1",
      REL2CONTRT = "  No. of patients with treatment received for AE related to study drug 2",
      REL1SER = "  No. of patients with serious AE related to study drug 1",
      REL2SER = "  No. of patients with serious AE related to study drug 2"
    )

  adae_mult <- adae_mult %>%
    mutate(
      AETOXGR = forcats::fct_recode(AETOXGR,
        "Grade 1" = "1",
        "Grade 2" = "2",
        "Grade 3" = "3",
        "Grade 4" = "4",
        "Grade 5 (fatal outcome)" = "5"
      )
    )

  aesi_vars <- c(
    "WD1", "WD2", "DSM1", "DSM2", "CONTRT", "ALL_RESOLVED", "ALL_RESOLVED_WD1", "ALL_RESOLVED_WD2",
    "ALL_RESOLVED_DSM1", "ALL_RESOLVED_DSM2", "ALL_RESOLVED_CONTRT", "NOT_RESOLVED", "NOT_RESOLVED_WD1",
    "NOT_RESOLVED_WD2", "NOT_RESOLVED_DSM1", "NOT_RESOLVED_DSM2", "NOT_RESOLVED_CONTRT", "SER", "SERWD1", "SERWD2",
    "SERDSM1", "SERDSM2", "SERCONTRT", "REL1", "REL1WD1", "REL1WD2", "REL1DSM1", "REL1DSM2", "REL1CONTRT", "REL1SER",
    "REL2", "REL2WD1", "REL2WD2", "REL2DSM1", "REL2DSM2", "REL2CONTRT", "REL2SER"
  )

  lyt_adae_mult <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    count_patients_with_event(
      vars = "USUBJID",
      filters = c("STUDYID" = as.character(unique(adae_mult$STUDYID))),
      denom = "N_col",
      .labels = c(count_fraction = "Total number of patients with at least one AE")
    ) %>%
    count_values(
      "STUDYID",
      values = as.character(unique(adae_mult$STUDYID)),
      .stats = "count",
      .labels = c(count = "Total number of AEs"),
      table_names = "total_aes"
    ) %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      var_labels = "Total number of patients with at least one AE by worst grade",
      show_labels = "visible"
    ) %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = var_labels(adae_mult[, aesi_vars]),
      denom = "N_col"
    )

  result <- build_table(lyt_adae_mult, df = adae_mult, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET01_AESI Variant 4 works as expected", {
  adsl <- filter(adsl_raw, SAFFL == "Y")
  adae <- filter(adae_raw, ANL01FL == "Y" & SAFFL == "Y")

  adsl <- df_explicit_na(adsl)
  adae <- df_explicit_na(adae)

  stack_adae_by_smq <- function(adae, smq) {
    adae_labels <- c(var_labels(adae), "Standardized MedDRA Query")
    l_df <- lapply(smq, function(ae_grp) {
      ae_scope <- stringr::str_replace(ae_grp, "NAM", "SC")
      keep <- adae[[ae_grp]] != "<Missing>"
      df <- adae[keep, ]
      if (substr(ae_grp, 1, 3) == "SMQ") {
        df[["SMQ"]] <- aesi_label(as.character(df[[ae_grp]]), scope = as.character(df[[ae_scope]]))
      } else {
        df[["SMQ"]] <- df[[ae_grp]]
      }
      df
    })
    result <- do.call(rbind, l_df)
    var_labels(result) <- adae_labels
    result
  }

  adae_smq <- stack_adae_by_smq(adae, c("SMQ01NAM", "SMQ02NAM", "CQ01NAM"))

  not_resolved <- adae_smq %>%
    filter(!(AEOUT %in% c("RECOVERED/RESOLVED", "FATAL", "RECOVERED/RESOLVED WITH SEQUELAE"))) %>%
    distinct(USUBJID) %>%
    mutate(
      NOT_RESOLVED = "Y"
    )

  adae_smq <- adae_smq %>%
    left_join(not_resolved, by = c("USUBJID")) %>%
    mutate(
      ALL_RESOLVED = is.na(NOT_RESOLVED),
      NOT_RESOLVED = !is.na(NOT_RESOLVED)
    ) %>%
    var_relabel(
      ALL_RESOLVED = "Total number of patients with all non-fatal AEs resolved",
      NOT_RESOLVED = "Total number of patients with at least one non-fatal unresolved or ongoing AE"
    )

  adae_smq <- adae_smq %>%
    mutate(
      AEDECOD = as.character(AEDECOD),
      WD = AEACN == "DRUG WITHDRAWN",
      DSM = AEACN %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
      CONTRT = AECONTRT == "Y",
      SER = AESER == "Y",
      REL = AEREL == "Y",
      ALL_RESOLVED_WD = WD == TRUE & ALL_RESOLVED == TRUE,
      ALL_RESOLVED_DSM = DSM == TRUE & ALL_RESOLVED == TRUE,
      ALL_RESOLVED_CONTRT = CONTRT == TRUE & ALL_RESOLVED == TRUE,
      NOT_RESOLVED_WD = WD == TRUE & NOT_RESOLVED == TRUE,
      NOT_RESOLVED_DSM = DSM == TRUE & NOT_RESOLVED == TRUE,
      NOT_RESOLVED_CONTRT = CONTRT == TRUE & NOT_RESOLVED == TRUE,
      SERWD = AESER == "Y" & AEACN == "DRUG WITHDRAWN",
      SERCONTRT = AECONTRT == "Y" & AESER == "Y",
      SERDSM = AESER == "Y" & AEACN %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
      RELWD = AEREL == "Y" & AEACN == "DRUG WITHDRAWN",
      RELDSM = AEREL == "Y" & AEACN %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
      RELCONTRT = AECONTRT == "Y" & AEREL == "Y",
      RELSER = AESER == "Y" & AEREL == "Y"
    ) %>%
    var_relabel(
      WD = "Total number of patients with study drug withdrawn due to AE",
      DSM = "Total number of patients with dose modified/interrupted due to AE",
      CONTRT = "Total number of patients with treatment received for AE",
      SER = "Total number of patients with at least one serious AE",
      REL = "Total number of patients with at least one related AE",
      ALL_RESOLVED_WD = "  No. of patients with study drug withdrawn due to resolved AE",
      ALL_RESOLVED_DSM = "  No. of patients with dose modified/interrupted due to resolved AE",
      ALL_RESOLVED_CONTRT = "  No. of patients with treatment received for resolved AE",
      NOT_RESOLVED_WD = "  No. of patients with study drug withdrawn due to unresolved or ongoing AE",
      NOT_RESOLVED_DSM = "  No. of patients with dose modified/interrupted due to unresolved or ongoing AE",
      NOT_RESOLVED_CONTRT = "  No. of patients with treatment received for unresolved or ongoing AE",
      SERWD = "  No. of patients with study drug withdrawn due to serious AE",
      SERDSM = "  No. of patients with dose modified/interrupted due to serious AE",
      SERCONTRT = "  No. of patients with treatment received for serious AE",
      RELWD = "  No. of patients with study drug withdrawn due to related AE",
      RELDSM = "  No. of patients with dose modified/interrupted due to related AE",
      RELCONTRT = "  No. of patients with treatment received for related AE",
      RELSER = "  No. of patients with serious, related AE"
    )

  adae_smq <- adae_smq %>%
    mutate(
      AETOXGR = forcats::fct_recode(AETOXGR,
        "Grade 1" = "1",
        "Grade 2" = "2",
        "Grade 3" = "3",
        "Grade 4" = "4",
        "Grade 5 (fatal outcome)" = "5"
      )
    )

  split_fun <- remove_split_levels("<Missing>")
  aesi_vars <- c("WD", "DSM", "CONTRT", "ALL_RESOLVED", "NOT_RESOLVED", "SER", "REL")

  lyt_adae <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_rows_by(
      "SMQ",
      child_labels = "visible",
      nested = FALSE,
      indent_mod = -1L,
      split_fun = split_fun,
      split_label = "Standardized MedDRA Query",
      label_pos = "topleft"
    ) %>%
    count_patients_with_event(
      vars = "USUBJID",
      filters = c("STUDYID" = as.character(unique(adae_smq$STUDYID))),
      denom = "N_col",
      .labels = c(count_fraction = "Total number of patients with at least one AE")
    ) %>%
    count_values(
      "STUDYID",
      values = as.character(unique(adae_smq$STUDYID)),
      .stats = "count",
      .labels = c(count = "Total number of AEs"),
      table_names = "total_aes"
    ) %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      var_labels = "Total number of patients with at least one AE by worst grade",
      .show_labels = "visible"
    ) %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = var_labels(adae_smq[, aesi_vars]),
      denom = "N_col"
    )
  result <- build_table(lyt_adae, df = adae_smq, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})