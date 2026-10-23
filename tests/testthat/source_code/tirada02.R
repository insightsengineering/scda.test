library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TIRADA02"
fileid <- write_path(opath, tblid)
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

popfl <- "SAFFL" # Safety Analysis Set flag
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"
parqual_value <- "XANOMELINE"
ae_filter_exp <- expr(toupper(AESCAT) == toupper("infusion related reaction")) # user sets this to match ADAE.AESCAT
sev_grade_fl <- "AESEV"
# sev_grade_fl <- "AETOXGRN"
sev_grade_lbl <- "severe"
# sev_grade_lbl <- ">=grade 3"
adexsum_tnumd_paramcd <- "TNUMDOS" # set to TNUMDSSx as needed
dosedt_var <- "DOSEDT" # set to DOSSxDT as needed
combined_colspan_trt <- TRUE
include_ctrl_grp <- FALSE # set to FALSE to exclude control group (e.g. Placebo) from the table
study_agent <- "active study agent"
agent_antibody_label <- sprintf("antibodies to %s", study_agent)

################################################################################
# Process data:
################################################################################

# create filter for severity
sev_filter_exp <- if (sev_grade_fl == "AESEV") {
  expr(toupper(!!sym(sev_grade_fl)) == toupper(sev_grade_lbl))
} else {
  expr(!!sym(sev_grade_fl) >= 3)
}

# Read all raw datasets once; apply only population/key filters here
adsl_raw <- adsl_jnj |>
  filter(.data[[popfl]] == "Y")

adishum_raw <- adishum_jnj |>
  filter(
    .data[[popfl]] == "Y",
    toupper(PARQUAL) == toupper(parqual_value)
  )

adae_raw <- adae_jnj |>
  filter(
    .data[[popfl]] == "Y",
    !!ae_filter_exp
  )

adexsum_raw <- adexsum_jnj |>
  filter(
    .data[[popfl]] == "Y",
    toupper(PARAMCD) == toupper(adexsum_tnumd_paramcd)
  ) |>
  select(USUBJID, INFUSAN = AVAL)

# ADSL: Derive analysis-ready adsl with colspan variable
adsl <- adsl_raw |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c(
        "Xanomeline Low Dose",
        "Xanomeline High Dose",
        "Placebo"
      )
    )
  ) |>
  create_colspan_var(
    non_active_grp = ctrl_grp,
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = study_agent |>
      stringi::stri_trans_totitle(),
    colspan_var = "colspan_trt",
    trt_var = trtvar
  ) |>
  select(
    USUBJID,
    all_of(trtvar),
    colspan_trt
  )

# ADISHUM: one row per subject; derive antibody status flags and carry IMEVFL
adishum <- adishum_raw |>
  select(USUBJID, ADATRES, IMEVFL) |>
  filter(!is.na(ADATRES)) |>
  distinct(USUBJID, .keep_all = TRUE) |>
  mutate(
    adatre_flg = if_else(toupper(ADATRES) == "POSITIVE", "Y", "N"),
    adantre_flg = if_else(toupper(ADATRES) == "NEGATIVE", "Y", "N")
  ) |>
  select(USUBJID, adatre_flg, adantre_flg, IMEVFL)

# ADAE: subject-level flags (distinct USUBJID per flag)
# NOTE: always count distinct subjects after subsetting on AESCAT + ADATRES + applicable attribute
adae_irr <- adae_raw |>
  distinct(USUBJID) |>
  mutate(infureact_flg = "Y") # any infusion-related reaction

# Purpose: flag subjects meeting severity/grade threshold per sev_filter_exp
adae_sev <- adae_raw |>
  filter(!!sev_filter_exp) |>
  distinct(USUBJID) |>
  mutate(sev_flg = "Y")

adae_ser <- adae_raw |>
  filter(toupper(AESER) == "Y") |>
  distinct(USUBJID) |>
  mutate(ser_flg = "Y") # AESER=Y

adae_treat_discon <- adae_raw |>
  filter(toupper(TRDISCFL) == "Y") |>
  distinct(USUBJID) |>
  mutate(treat_discon_flg = "Y") # TRDISCFL=Y

adae_dosct <- adexsum_raw |>
  select(USUBJID) |>
  left_join(
    adae_raw |>
      group_by(USUBJID) |>
      summarise(
        n_dosedt_num = n_distinct(.data[[dosedt_var]], na.rm = TRUE),
        .groups = "drop"
      ),
    by = "USUBJID"
  ) |>
  mutate(n_dosedt_num = if_else(is.na(n_dosedt_num), 0, n_dosedt_num))

# Final join: adsl -> adishum -> imevfl -> all adae flags -> adexsum
# Each auxiliary dataset has one row per subject — no many-to-many
join_list <- list(
  adae_irr,
  adae_sev,
  adae_ser,
  adae_treat_discon,
  adae_dosct,
  adexsum_raw
)

adishum <- adsl |>
  left_join(adishum, by = "USUBJID") |>
  purrr::reduce(join_list, ~ left_join(.x, .y, by = "USUBJID"), .init = _) |>
  distinct(USUBJID, .keep_all = TRUE) |>
  mutate(
    across(
      c(infureact_flg, sev_flg, ser_flg, treat_discon_flg),
      ~ if_else(is.na(.), "N", as.character(.))
    )
  )

# Section --- Build analysis flags from joined columns ------------
# Section 1 --- Adatre Positive (Treatment-Emergent Antibody Positive) ------------
adishum <- adishum |>
  mutate(
    # infureact_pos_flg: any infusion-related reaction in adae (AESCAT filter already applied)
    infureact_pos_flg = case_when(
      adatre_flg == "Y" & infureact_flg == "Y" ~ "Y",
      adatre_flg == "Y" ~ "N",
      TRUE ~ NA_character_
    ),
    # sev_pos_flg: per sev_filter_exp
    sev_pos_flg = case_when(
      adatre_flg == "Y" & sev_flg == "Y" ~ "Y",
      adatre_flg == "Y" ~ "N",
      TRUE ~ NA_character_
    ),
    # ser_pos_flg: AESER=Y
    ser_pos_flg = case_when(
      adatre_flg == "Y" & ser_flg == "Y" ~ "Y",
      adatre_flg == "Y" ~ "N",
      TRUE ~ NA_character_
    ),
    # treat_discon_pos_flg: TRDISCFL=Y
    treat_discon_pos_flg = case_when(
      adatre_flg == "Y" & treat_discon_flg == "Y" ~ "Y",
      adatre_flg == "Y" ~ "N",
      TRUE ~ NA_character_
    ),
    # infusan_pos_flg: total infusions from ADEXSUM for positive subjects (denominator for footnote e)
    infusan_pos_flg = if_else(adatre_flg == "Y", INFUSAN, NA_real_),
    # infusarn_pos_flg: distinct IRR infusion dates for positive subjects (numerator for footnote e)
    infusarn_pos_flg = if_else(adatre_flg == "Y", as.numeric(n_dosedt_num), NA_real_)
  )

# Section 2 --- Adantre Negative (Treatment-Emergent Antibody Negative) ------------
adishum <- adishum |>
  mutate(
    # infureact_neg_flg: any infusion-related reaction in adae
    infureact_neg_flg = case_when(
      adantre_flg == "Y" & infureact_flg == "Y" ~ "Y",
      adantre_flg == "Y" ~ "N",
      TRUE ~ NA_character_
    ),
    # sev_neg_flg: per sev_filter_exp
    sev_neg_flg = case_when(
      adantre_flg == "Y" & sev_flg == "Y" ~ "Y",
      adantre_flg == "Y" ~ "N",
      TRUE ~ NA_character_
    ),
    # ser_neg_flg: AESER=Y
    ser_neg_flg = case_when(
      adantre_flg == "Y" & ser_flg == "Y" ~ "Y",
      adantre_flg == "Y" ~ "N",
      TRUE ~ NA_character_
    ),
    # treat_discon_neg_flg: TRDISCFL=Y
    treat_discon_neg_flg = case_when(
      adantre_flg == "Y" & treat_discon_flg == "Y" ~ "Y",
      adantre_flg == "Y" ~ "N",
      TRUE ~ NA_character_
    ),
    # infusan_neg_flg: total infusions from ADEXSUM for negative subjects (denominator for footnote e)
    infusan_neg_flg = if_else(
      adantre_flg == "Y",
      INFUSAN,
      NA_real_
    ),
    # infusarn_neg_flg: distinct IRR infusion dates for negative subjects (numerator for footnote e)
    infusarn_neg_flg = if_else(
      adantre_flg == "Y",
      as.numeric(n_dosedt_num),
      NA_real_
    )
  )

# final output needs only these columns
adishum <- adishum |>
  select(
    USUBJID,
    all_of(trtvar),
    colspan_trt,
    IMEVFL,
    adatre_flg,
    adantre_flg,
    infureact_pos_flg,
    sev_pos_flg,
    ser_pos_flg,
    treat_discon_pos_flg,
    infusan_pos_flg,
    infusarn_pos_flg,
    infureact_neg_flg,
    sev_neg_flg,
    ser_neg_flg,
    treat_discon_neg_flg,
    infusan_neg_flg,
    infusarn_neg_flg
  ) |>
  unique()

# Section --- Prepare Rtables Helpers ------------
# add the Combined column via split function
if (combined_colspan_trt) {
  add_combo <- add_combo_facet(
    "Combined",
    label = "Combined",
    levels = c("Xanomeline Low Dose", "Xanomeline High Dose") # UPDATE
  )

  rm_combo_from_placebo <- cond_rm_facets(
    facets = "Combined",
    ancestor_pos = NA,
    value = " ",
    split = "colspan_trt"
  )

  mysplit <- make_split_fun(post = list(add_combo, rm_combo_from_placebo))
}

# colspan map built from adsl, not adishum
colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = study_agent |>
    stringi::stri_trans_totitle(),
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

if (!include_ctrl_grp) {
  colspan_trt_map <- colspan_trt_map |>
    filter(.data[[trtvar]] != ctrl_grp)
}

################################################################################
# Define layout and build table:
################################################################################
lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) |>
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(
      map = colspan_trt_map
    )
  ) |>
  split_cols_by(
    trtvar,
    split_fun = mysplit
  ) |>
  analyze(
    "IMEVFL",
    afun = a_freq_j,
    show_labels = "hidden",
    section_div = " ",
    extra_args = list(
      label = "Subjects with evaluable samples~[super a]",
      val = "Y",
      .stats = c("count_unique")
    )
  ) |>
  # Section 1 --- Adatre Positive (Treatment-Emergent Antibody Positive) ------------
  analyze(
    "adatre_flg",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      label = sprintf(
        "Subjects positive for treatment-emergent %s~[super b,d]",
        agent_antibody_label
      ),
      val = "Y",
      .stats = c("count_unique")
    )
  ) |>
  analyze(
    "infureact_pos_flg",
    afun = a_freq_j,
    show_labels = "hidden",
    indent_mod = 1L,
    extra_args = list(
      label = "Subjects with infusion-related reaction, n (%)",
      val = "Y",
      .stats = c("count_unique_fraction"),
      denom = "n_df"
    )
  ) |>
  analyze(
    "sev_pos_flg",
    afun = a_freq_j,
    show_labels = "hidden",
    indent_mod = 1L,
    extra_args = list(
      label = sprintf(
        "Subjects with %s infusion-related reaction, n (%%)",
        sev_grade_lbl
      ),
      val = "Y",
      .stats = c("count_unique_fraction"),
      denom = "n_df"
    )
  ) |>
  analyze(
    "ser_pos_flg",
    afun = a_freq_j,
    show_labels = "hidden",
    indent_mod = 1L,
    extra_args = list(
      label = "Subjects with serious infusion-related reaction, n (%)",
      val = "Y",
      .stats = c("count_unique_fraction"),
      denom = "n_df"
    )
  ) |>
  analyze(
    "treat_discon_pos_flg",
    afun = a_freq_j,
    show_labels = "hidden",
    indent_mod = 1L,
    extra_args = list(
      label = "Subjects with infusion-related reaction leading to treatment discontinuation, n (%)",
      val = "Y",
      .stats = c("count_unique_fraction"),
      denom = "n_df"
    )
  ) |>
  analyze(
    "infusan_pos_flg",
    afun = a_sum_ratio_j,
    show_labels = "hidden",
    indent_mod = 1L,
    extra_args = list(
      .stats = "sum_unique",
      .labels = c(
        sum_unique = sprintf(
          "Total number of %s infusions",
          study_agent
        )
      ),
      id_var = "USUBJID"
    )
  ) |>
  analyze(
    "infusarn_pos_flg",
    afun = a_sum_ratio_j,
    show_labels = "hidden",
    indent_mod = 2L,
    section_div = " ",
    extra_args = list(
      .stats = "ratio_unique",
      .labels = c(
        ratio_unique = "Infusions with infusion-related reactions~[super e], n (%)"
      ),
      denom_by = "infusan_pos_flg",
      id_var = "USUBJID"
    )
  ) |>
  # Section 2 --- Adantre Negative (Treatment-Emergent Antibody Negative) ------------
  analyze(
    "adantre_flg",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      label = sprintf(
        "Subjects negative for treatment-emergent %s~[super c,d]",
        agent_antibody_label
      ),
      val = "Y",
      .stats = c("count_unique")
    )
  ) |>
  analyze(
    "infureact_neg_flg",
    afun = a_freq_j,
    show_labels = "hidden",
    indent_mod = 1L,
    extra_args = list(
      label = "Subjects with infusion-related reaction, n (%)",
      val = "Y",
      .stats = c("count_unique_fraction"),
      denom = "n_df"
    )
  ) |>
  analyze(
    "sev_neg_flg",
    afun = a_freq_j,
    show_labels = "hidden",
    indent_mod = 1L,
    extra_args = list(
      label = sprintf(
        "Subjects with %s infusion-related reaction, n (%%)",
        sev_grade_lbl
      ),
      val = "Y",
      .stats = c("count_unique_fraction"),
      denom = "n_df"
    )
  ) |>
  analyze(
    "ser_neg_flg",
    afun = a_freq_j,
    show_labels = "hidden",
    indent_mod = 1L,
    extra_args = list(
      label = "Subjects with serious infusion-related reaction, n (%)",
      val = "Y",
      .stats = c("count_unique_fraction"),
      denom = "n_df"
    )
  ) |>
  analyze(
    "treat_discon_neg_flg",
    afun = a_freq_j,
    show_labels = "hidden",
    indent_mod = 1L,
    extra_args = list(
      label = "Subjects with infusion-related reaction leading to treatment discontinuation, n (%)",
      val = "Y",
      .stats = c("count_unique_fraction"),
      denom = "n_df"
    )
  ) |>
  analyze(
    "infusan_neg_flg",
    afun = a_sum_ratio_j,
    show_labels = "hidden",
    indent_mod = 1L,
    extra_args = list(
      .stats = "sum_unique",
      .labels = c(
        sum_unique = sprintf(
          "Total number of %s infusions",
          study_agent
        )
      ),
      id_var = "USUBJID"
    )
  ) |>
  analyze(
    "infusarn_neg_flg",
    afun = a_sum_ratio_j,
    show_labels = "hidden",
    indent_mod = 2L,
    section_div = " ",
    extra_args = list(
      .stats = "ratio_unique",
      .labels = c(
        ratio_unique = "Infusions with infusion-related reactions~[super e], n (%)"
      ),
      denom_by = "infusan_neg_flg",
      id_var = "USUBJID"
    )
  )

# Section --- Create Table Shell ------------

result <- build_table(lyt, adishum, alt_counts_df = adsl, round_type = "sas")

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table:
################################################################################


colwidth <- c(64, 21, 21, 21)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "landscape")
