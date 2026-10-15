library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TIRADA02b_titer"
fileid <- write_path(opath, tblid)
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

popfl <- "SAFFL" # Safety Analysis Set flag
parqual_value <- "XANOMELINE"
ae_filter_exp <- expr(toupper(AESCAT) == toupper("infusion site reaction")) # user sets this to match ADAE.AESCAT
sev_grade_fl <- "AESEV"
# sev_grade_fl <- "AETOXGRN"
sev_grade_lbl <- "severe"
# sev_grade_lbl <- ">=grade 3"
adexsum_tnumd_paramcd <- "TNUMDOS" # set to TNUMDSSx as needed
dosedt_var <- "DOSEDT" # set to DOSSxDT as needed
study_agent <- "active study agent"

################################################################################
# Process data:
################################################################################

# create filter for severity
sev_filter_exp <- if (sev_grade_fl == "AESEV") {
  expr(toupper(!!sym(sev_grade_fl)) == toupper(sev_grade_lbl))
} else {
  expr(!!sym(sev_grade_fl) >= 3)
}

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

# defining column labels and levels
neg_lbl <- sprintf(
  "Negative for Treatment-emergent Antibodies to %s~[super a]",
  study_agent
)
pos_lbl <- sprintf(
  "Positive for Treatment-emergent Antibodies to %s~[super b]",
  study_agent
)
titer_lbl <- sprintf(
  "Peak Titers for Subjects Positive for Treatment-emergent Antibodies to %s",
  study_agent
)
adatrept_lvl <- c("<10", "10 to <100", "100 to <1000", ">=1000")

# Section --- Build column split variables ------------

# Subjects negative for treatment-emergent antibodies
neg_col_df <- adishum_raw |>
  filter(toupper(ADATRES) == "NEGATIVE") |>
  select(USUBJID) |>
  mutate(neg_col = neg_lbl) |>
  unique()

# Subjects positive for treatment-emergent antibodies
pos_col_df <- adishum_raw |>
  filter(toupper(ADATRES) == "POSITIVE") |>
  select(USUBJID) |>
  mutate(pos_col = pos_lbl) |>
  unique()

# Peak titer category column format
titer_col_df <- adishum_raw |>
  select(USUBJID, ADATREPT) |>
  mutate(
    titer_col = if_else(!is.na(ADATREPT), titer_lbl, NA_character_),
    ADATREPT = factor(ADATREPT, levels = adatrept_lvl)
  ) |>
  distinct(USUBJID, .keep_all = TRUE)

# ADSL: attach col split vars for column layout; used as alt_counts_df
adsl <- adsl_raw |>
  select(USUBJID) |>
  left_join(neg_col_df, by = "USUBJID") |>
  left_join(pos_col_df, by = "USUBJID") |>
  left_join(titer_col_df, by = "USUBJID")

# Section --- ADISHUM: one row per subject; carry IMEVFL ------------
adishum <- adishum_raw |>
  select(USUBJID, IMEVFL) |>
  filter(IMEVFL == "Y") |>
  distinct(USUBJID, .keep_all = TRUE)

# Section --- ADAE: subject-level flags (distinct USUBJID per flag) ------------
# NOTE: always count distinct subjects after subsetting on AESCAT + applicable attribute
adae_irr <- adae_raw |>
  distinct(USUBJID) |>
  mutate(infureact_flg = "Y") # any infusion site reaction

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

# Section --- Final join: adsl -> adishum -> adae flags -> adexsum ------------
# Each auxiliary dataset is already one row per subject — no many-to-many risk
# adishum: distinct(USUBJID) above; adae_*: distinct(USUBJID) per flag;
# adae_dosct: one row per subject from adexsum_raw spine; adexsum_raw: select(USUBJID, INFUSAN)
join_list <- list(
  adishum,
  adae_irr,
  adae_sev,
  adae_ser,
  adae_treat_discon,
  adae_dosct,
  adexsum_raw
)

adishum <- adsl |>
  purrr::reduce(
    join_list,
    ~ left_join(.x, .y, by = "USUBJID"),
    .init = _
  ) |>
  mutate(
    across(
      c(infureact_flg, sev_flg, ser_flg, treat_discon_flg),
      ~ if_else(is.na(.), "N", as.character(.))
    ),
    # infusan_num_flg: total active agent infusions from adexsum (denominator footnote e)
    infusan_num_flg = INFUSAN,
    # infusarn_num_flg: distinct infusion site reaction infusion dates (numerator footnote e)
    infusarn_num_flg = as.numeric(n_dosedt_num)
  ) |>
  select(
    USUBJID,
    IMEVFL,
    infureact_flg,
    sev_flg,
    ser_flg,
    treat_discon_flg,
    infusan_num_flg,
    infusarn_num_flg,
    neg_col,
    pos_col,
    titer_col,
    ADATREPT
  ) |>
  unique()

################################################################################
# Define layout and build table:
################################################################################
lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) |>
  split_cols_by(
    var = "neg_col",
    split_fun = keep_split_levels(only = neg_lbl),
    nested = FALSE
  ) |>
  split_cols_by(
    var = "pos_col",
    split_fun = keep_split_levels(only = pos_lbl),
    nested = FALSE
  ) |>
  split_cols_by(
    var = "titer_col",
    split_fun = keep_split_levels(only = titer_lbl),
    nested = FALSE
  ) |>
  split_cols_by(
    var = "ADATREPT"
  ) |>
  analyze(
    "IMEVFL",
    afun = a_freq_j,
    show_labels = "hidden",
    section_div = " ",
    extra_args = list(
      label = "Subjects with evaluable samples~[super c]",
      val = "Y",
      .stats = c("count_unique")
    )
  ) |>
  analyze(
    "infureact_flg",
    afun = a_freq_j,
    show_labels = "hidden",
    indent_mod = 1L,
    extra_args = list(
      label = "Subjects with infusion site reaction~[super d], n (%)",
      val = "Y",
      .stats = c("count_unique_fraction"),
      denom = "n_df"
    )
  ) |>
  analyze(
    "sev_flg",
    afun = a_freq_j,
    show_labels = "hidden",
    indent_mod = 1L,
    extra_args = list(
      label = sprintf(
        "Subjects with %s infusion site reaction~[super d], n (%%)",
        sev_grade_lbl
      ),
      val = "Y",
      .stats = c("count_unique_fraction"),
      denom = "n_df"
    )
  ) |>
  analyze(
    "ser_flg",
    afun = a_freq_j,
    show_labels = "hidden",
    indent_mod = 1L,
    extra_args = list(
      label = "Subjects with serious infusion site reaction~[super d], n (%)",
      val = "Y",
      .stats = c("count_unique_fraction"),
      denom = "n_df"
    )
  ) |>
  analyze(
    "treat_discon_flg",
    afun = a_freq_j,
    show_labels = "hidden",
    indent_mod = 1L,
    extra_args = list(
      label = "Subjects with infusion site reaction leading to treatment discontinuation~[super d], n (%)",
      val = "Y",
      .stats = c("count_unique_fraction"),
      denom = "n_df"
    )
  ) |>
  analyze(
    "infusan_num_flg",
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
    "infusarn_num_flg",
    afun = a_sum_ratio_j,
    show_labels = "hidden",
    indent_mod = 2L,
    extra_args = list(
      .stats = "ratio_unique",
      .labels = c(
        ratio_unique = "Infusions with infusion site reactions~[super e], n (%)"
      ),
      denom_by = "infusan_num_flg",
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


colwidth <- c(64, 21, 21, 21, 21, 21, 21)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "landscape")
