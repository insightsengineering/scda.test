library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TIRADA01"
fileid <- write_path(opath, tblid)
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

popfl <- "IMFL" # Immunogenicity Analysis Set flag
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"
parqual_value <- "XANOMELINE"
combined_colspan_trt <- TRUE
include_ctrl_grp <- FALSE # set to FALSE to exclude control group (e.g. Placebo) from the table
study_agent <- "active study agent"
agent_antibody_label <- sprintf("antibodies to %s", study_agent)

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

# each has a flag PARAMCD and a titer PARAMCD
ada_sections <- list(
  list(flag = "ADABL", flag_col = "adabl_flag", titer = "ADABLT", titer_col = "adablt_titer"),
  list(flag = "ADATRB", flag_col = "adatrb_flag", titer = "ADATRBT", titer_col = "adatrbt_titer"),
  list(flag = "ADANTRB", flag_col = "adantrb_flag", titer = "ADANTRBT", titer_col = "adantrbt_titer"),
  list(flag = "ADATRI", flag_col = "adatri_flag", titer = "ADATRIPT", titer_col = "adatript_titer"),
  list(flag = "ADATRE", flag_col = "adatre_flag", titer = "ADATREPT", titer_col = "adatrept_titer"),
  list(flag = "ADANTRE", flag_col = "adantre_flag", titer = NULL, titer_col = NULL)
)

# join all flag and titer columns onto one subject-level dataset
for (section in ada_sections) {
  flag_df <- adishum |>
    filter(PARAMCD == section$flag) |>
    select(USUBJID, !!section$flag_col := AVALC)

  adishum <- adishum |>
    left_join(flag_df, by = "USUBJID")

  if (!is.null(section$titer)) {
    titer_df <- adishum |>
      filter(PARAMCD == section$titer) |>
      select(USUBJID, !!section$titer_col := AVALC)

    adishum <- adishum |>
      left_join(titer_df, by = "USUBJID")
  }
}

# convert all titer values in an ordered factor
adishum <- adishum |>
  mutate(
    across(
      ends_with("_flag"),
      ~ dplyr::if_else(is.na(.), "N", as.character(.))
    ),
    across(
      ends_with("_titer"),
      ~ {
        vals <- unique(.[!is.na(.)])
        vals_matched <- vals[grepl(":(\\d+)", vals)]
        nums <- vals_matched |>
          sub(
            pattern = ".*:(\\d+).*",
            replacement = "\\1"
          ) |>
          as.numeric()
        ordered_levels <- vals_matched[order(nums)]
        factor(., levels = ordered_levels)
      }
    )
  )

# peak titer group from AVALCAT1 (not AVALC)
adatrept_cat_df <- adishum |>
  filter(PARAMCD == "ADATREPT") |>
  select(USUBJID, adatrept_cat = AVALCAT1)

adishum <- adishum |>
  left_join(adatrept_cat_df, by = "USUBJID") |>
  mutate(
    adatrept_cat = case_when(
      adatre_flag == "Y" & !is.na(adatrept_cat) ~ as.character(adatrept_cat),
      adatre_flag == "Y" & is.na(adatrept_cat) ~ "No titer",
      TRUE ~ NA_character_
    ),
    adatrept_cat = factor(
      adatrept_cat,
      levels = c("<10", "10 to <100", "100 to <1000", ">=1000", "No titer")
    )
  )

# programming note 3: check if any subjects positive per section
has_adabl <- any(adishum$adabl_flag == "Y", na.rm = TRUE)
has_adatrb <- any(adishum$adatrb_flag == "Y", na.rm = TRUE)
has_adantrb <- any(adishum$adantrb_flag == "Y", na.rm = TRUE)
has_adatri <- any(adishum$adatri_flag == "Y", na.rm = TRUE)
has_adatre <- any(adishum$adatre_flag == "Y", na.rm = TRUE)

# final output needs only these columns
adishum <- adishum |>
  select(
    USUBJID,
    all_of(trtvar), # TRT01A
    colspan_trt, # column split
    # flag columns
    adabl_flag,
    adatrb_flag,
    adantrb_flag,
    adatri_flag,
    adatre_flag,
    adantre_flag,
    # titer columns
    adablt_titer,
    adatrbt_titer,
    adantrbt_titer,
    adatript_titer,
    adatrept_titer,
    # peak titer group
    adatrept_cat
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
  append_topleft(
    c("Antibody Status", indent_string("Titer", 1L))
  ) |>
  # define column splits
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) |>
  split_cols_by(trtvar, split_fun = mysplit) |>
  # define analysis variable
  # Section 1: baseline positive flag row
  analyze(
    "adabl_flag",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      label = sprintf(
        "Subjects positive for %s at baseline~[super a], n (%%)",
        agent_antibody_label
      ),
      val = "Y",
      .stats = c("count_unique_fraction")
    )
  )

# programming note 3: titer rows only if positives exist
if (has_adabl) {
  lyt <- lyt |>
    analyze(
      "adablt_titer",
      afun = a_freq_j,
      show_labels = "visible",
      var_labels = "Baseline titers",
      indent_mod = 1L,
      section_div = " ",
      extra_args = list(.stats = c("count_unique"))
    )
}

lyt <- lyt |>
  # Section 2: treatment-boosted flag row
  analyze(
    "adatrb_flag",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      label = sprintf(
        "Subjects positive for %s at baseline who were treatment-boosted for %s~[super b], n (%%)",
        agent_antibody_label,
        agent_antibody_label
      ),
      val = "Y",
      .stats = c("count_unique_fraction")
    )
  )

if (has_adatrb) {
  lyt <- lyt |>
    analyze(
      "adatrbt_titer",
      afun = a_freq_j,
      show_labels = "visible",
      var_labels = "Baseline titers",
      indent_mod = 1L,
      section_div = " ",
      extra_args = list(.stats = c("count_unique"))
    )
}

lyt <- lyt |>
  # Section 3: not treatment-boosted flag row
  analyze(
    "adantrb_flag",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      label = sprintf(
        "Subjects positive for %s at baseline who were not treatment-boosted for %s~[super c], n (%%)",
        agent_antibody_label,
        agent_antibody_label
      ),
      val = "Y",
      .stats = c("count_unique_fraction")
    )
  )

if (has_adantrb) {
  lyt <- lyt |>
    analyze(
      "adantrbt_titer",
      afun = a_freq_j,
      show_labels = "visible",
      var_labels = "Baseline titers",
      section_div = " ",
      indent_mod = 1L,
      extra_args = list(.stats = c("count_unique"))
    )
}

lyt <- lyt |>
  # Section 4: treatment-induced flag row
  analyze(
    "adatri_flag",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      label = sprintf(
        "Subjects positive for treatment-induced %s~[super d], n (%%)",
        agent_antibody_label
      ),
      val = "Y",
      .stats = c("count_unique_fraction")
    )
  )

if (has_adatri) {
  lyt <- lyt |>
    analyze(
      "adatript_titer",
      afun = a_freq_j,
      show_labels = "visible",
      var_labels = "Peak titers",
      section_div = " ",
      indent_mod = 1L,
      extra_args = list(.stats = c("count_unique"))
    )
}

lyt <- lyt |>
  # Section 5: treatment-emergent flag row
  analyze(
    "adatre_flag",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      label = sprintf(
        "Subjects positive for treatment-emergent %s~[super e], n (%%)",
        agent_antibody_label
      ),
      val = "Y",
      .stats = c("count_unique_fraction")
    )
  )

if (has_adatre) {
  lyt <- lyt |>
    analyze(
      "adatrept_titer",
      afun = a_freq_j,
      show_labels = "visible",
      var_labels = "Peak titers",
      section_div = " ",
      indent_mod = 1L,
      extra_args = list(.stats = c("count_unique"))
    )
}

lyt <- lyt |>
  # Section 6: peak titer group — always shown
  analyze(
    "adatrept_cat",
    afun = a_freq_j,
    show_labels = "visible",
    var_labels = "Peak titer group~[super f]",
    section_div = " ",
    indent_mod = 1L,
    extra_args = list(
      .stats = c("count_unique_fraction"),
      denom = "n_df",
      excl_levels = "No titer"
    )
  ) |>
  # Section 7: negative for treatment-emergent flag row
  analyze(
    "adantre_flag",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      label = sprintf(
        "Subjects negative for treatment-emergent %s~[super g], n (%%)",
        agent_antibody_label
      ),
      val = "Y",
      .stats = c("count_unique_fraction")
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


colwidth <- c(64, 23, 23, 25)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "landscape")
