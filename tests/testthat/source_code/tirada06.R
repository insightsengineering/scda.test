library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TIRADA06"
fileid <- write_path(opath, tblid)
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

popfl <- "IMFL" # Immunogenicity Analysis Set flag
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"
parqual_value <- "XANOMELINE"
combined_colspan_trt <- TRUE
include_ctrl_grp <- FALSE # set to FALSE to exclude control group (e.g. Placebo) from the table
study_agent <- "active study agent" # study-specific name of the active study agent
agent_antibody_label <- sprintf("antibodies to %s", study_agent)

# optional row flags: set to FALSE to hide the corresponding row(s)
add_adapsp_row <- TRUE # subjects with persistent ADA response (section 2)
add_adatsp_row <- TRUE # subjects with transient ADA response (section 3)
add_adaund_row <- TRUE # subjects with undetermined ADA response (section 4)
add_pspdurw_section <- TRUE # duration of persistent treatment-induced ADA (section 7)
add_cv <- TRUE # cv (%) row in all continuous summary sections
add_interquartile_range <- TRUE # interquartile range row in all continuous summary sections

################################################################################
# Process data:
################################################################################

adsl <- adsl_jnj |>
  filter(.data[[popfl]] == "Y") |>
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

adishum_raw <- adishum_jnj

adishum <- adishum_raw |>
  filter(
    toupper(PARQUAL) == toupper(parqual_value)
  ) |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c(
        "Xanomeline Low Dose",
        "Xanomeline High Dose",
        "Placebo"
      )
    )
  )

# left join from adsl to include all eligible subjects
# colspan_trt and trtvar (with factor levels) come from adsl
adishum <- adsl |>
  left_join(
    adishum |>
      select(-all_of(trtvar)),
    by = "USUBJID"
  )

# Section --- Auxilary Datasets Needed For Final Dataset ------------
# extract flag columns (one row per subject) and join onto adishum
flag_paramcds <- c("ADATRI", "ADAPSP", "ADATSP", "ADAUND")

for (p in flag_paramcds) {
  col_name <- tolower(p)
  flag_df <- adishum |>
    filter(PARAMCD == p) |>
    select(USUBJID, !!col_name := AVALC)

  adishum <- adishum |>
    left_join(flag_df, by = "USUBJID") |>
    mutate(
      !!col_name := if_else(
        is.na(.data[[col_name]]),
        "N",
        .data[[col_name]]
      )
    )
}

# continuous variables: one row per subject per PARAMCD, join AVAL onto adishum
cont_paramcds <- c("TMOSADAW", "ADADURW", "PSPDURW")

for (p in cont_paramcds) {
  col_name <- tolower(p)
  cont_df <- adishum |>
    filter(PARAMCD == p) |>
    select(USUBJID, !!col_name := AVAL)

  adishum <- adishum |>
    left_join(cont_df, by = "USUBJID")
}

adishum <- adishum |>
  select(
    USUBJID,
    all_of(trtvar),
    colspan_trt,
    adatri,
    adapsp,
    adatsp,
    adaund,
    tmosadaw,
    adadurw,
    pspdurw
  ) |>
  # remove all complete duplicate rows
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
# shared extra_args for all continuous summary sections (sections 5, 6, 7)
# base stats always shown: n, mean (sd), median, min/max
cont_stats <- c("n", "mean_sd", "median", "range", if (add_cv) "cv", if (add_interquartile_range) "quantiles")

# indent levels for each stat row (0 = flush with section header, 1 = indented)
cont_indent_mods <- c("n" = 0L, "mean_sd" = 1L, "median" = 1L, "range" = 1L, "cv" = 1L, "quantiles" = 1L)

# display labels for stat rows that differ from rtables defaults
cont_labels <- c("n" = "N", "range" = "Min, max", "quantiles" = "Interquartile range")

dp <- 1
.formats_all <- junco:::fmt_spec_single_d(
  d = dp,
  stats_in = cont_stats,
  fmt_d_def = junco_def_d_all,
  fmt_d_in = NULL
)
fmt_details <- get_fmt_details(.formats_all, as_tibble = TRUE)

multi_stat_summary_args <- list(
  .stats = cont_stats,
  .labels = cont_labels,
  .formats = .formats_all,
  .indent_mods = cont_indent_mods,
  control = control_analyze_vars(
    quantiles = c(0.25, 0.75),
    quantile_type = 2
  )
)

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) |>
  # define column splits
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
  # Section 1: treatment-induced flag row
  analyze(
    "adatri",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      label = sprintf(
        "Subjects positive for treatment-induced %s~[super a]",
        agent_antibody_label
      ),
      val = "Y",
      .stats = c("count_unique")
    )
  )

# Section 2: persistent ADA response
if (add_adapsp_row) {
  lyt <- lyt |>
    analyze(
      "adapsp",
      afun = a_freq_j,
      show_labels = "hidden",
      indent_mod = 1L,
      extra_args = list(
        label = "Subjects with persistent ADA response~[super b]",
        val = "Y",
        .stats = c("count_unique_fraction")
      )
    )
}

# Section 3: transient ADA response
if (add_adatsp_row) {
  lyt <- lyt |>
    analyze(
      "adatsp",
      afun = a_freq_j,
      show_labels = "hidden",
      indent_mod = 1L,
      extra_args = list(
        label = "Subjects with transient ADA response~[super c]",
        val = "Y",
        .stats = c("count_unique_fraction")
      )
    )
}

# Section 4: undetermined ADA response
if (add_adaund_row) {
  lyt <- lyt |>
    analyze(
      "adaund",
      afun = a_freq_j,
      show_labels = "hidden",
      indent_mod = 1L,
      section_div = " ",
      extra_args = list(
        label = "Subjects with undetermined ADA response~[super d]",
        val = "Y",
        .stats = c("count_unique_fraction")
      )
    )
}

lyt <- lyt |>
  # Section 5: time to onset of treatment-induced ADA
  analyze(
    "tmosadaw",
    afun = a_summary,
    show_labels = "visible",
    section_div = " ",
    var_labels = "Time to onset of treatment-induced ADA~[super e], (weeks)",
    extra_args = multi_stat_summary_args
  ) |>
  # Section 6: duration of treatment-induced ADA
  analyze(
    "adadurw",
    afun = a_summary,
    show_labels = "visible",
    section_div = " ",
    var_labels = "Duration of treatment-induced ADA~[super f], (weeks)",
    extra_args = multi_stat_summary_args
  )

# Section 7: duration of persistent treatment-induced ADA
if (add_pspdurw_section) {
  lyt <- lyt |>
    analyze(
      "pspdurw",
      afun = a_summary,
      show_labels = "visible",
      section_div = " ",
      var_labels = "Duration of persistent treatment-induced ADA~[super b,f], (weeks)",
      extra_args = multi_stat_summary_args
    )
}

# Section --- Create Table Shell ------------

result <- build_table(lyt, adishum, alt_counts_df = adsl, round_type = "sas")

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table:
################################################################################


colwidth <- c(64, 31, 31, 31)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "landscape")
