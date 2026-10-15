library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TIRADA07"
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
study_specific_critical_var <- "AVAL" # continuous response variable (update per SAP)
study_agent <- "active study agent"

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
    non_active_grp_span_lbl = "Y",
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

# ordered time point levels from AVISIT + ATPT
avisit_atpt_lvls <- adishum |>
  filter(!is.na(AVISIT), !is.na(ATPT)) |>
  arrange(AVISITN, ATPTN) |>
  mutate(AVISIT_ATPT = paste(AVISIT, ATPT, sep = " - ")) |>
  pull(AVISIT_ATPT) |>
  unique()

# derive visit-level numerator and denominator vars masked to positive subgroup
adishum <- adishum |>
  mutate(
    AVISIT_ATPT = factor(
      if_else(!is.na(AVISIT) & !is.na(ATPT), paste(AVISIT, ATPT, sep = " - "), NA_character_),
      levels = avisit_atpt_lvls
    )
  )

# Section 2 --- Subjects positive for treatment-emergent antibodies (ADATRI OR ADATRB) ------------
# flag subjects with ADATRI=Y or ADATRB=Y (treatment-induced or treatment-boosted)
pos_subj <- adishum |>
  filter(
    (PARAMCD == "ADATRI" & AVALC == "Y") |
      (PARAMCD == "ADATRB" & AVALC == "Y")
  ) |>
  select(USUBJID) |>
  distinct() |>
  mutate(adatri_b_flag = "Y")

adishum <- adishum |>
  left_join(pos_subj, by = "USUBJID") |>
  mutate(adatri_b_flag = if_else(is.na(adatri_b_flag), "N", adatri_b_flag))

# Section ---- for calculating time point
time_point_filtered <- adishum |>
  filter(
    PARAMCD == "SUADAST" & AVALC == "POSITIVE"
  ) |>
  select(USUBJID, AVISIT, ATPT) |>
  distinct() |>
  mutate(pos_time_point_flag = factor("Y"))

imevfl_pos_filtered <- adishum |>
  filter(
    IMEVFL == "Y"
  ) |>
  select(USUBJID, AVISIT, ATPT) |>
  distinct() |>
  mutate(imevfl_pos = factor("Y"))

adishum <- adishum |>
  left_join(
    imevfl_pos_filtered,
    by = c("USUBJID", "AVISIT", "ATPT")
  ) |>
  left_join(
    time_point_filtered,
    by = c("USUBJID", "AVISIT", "ATPT")
  ) |>
  mutate(
    imevfl_pos = if_else(is.na(imevfl_pos), "N", "Y") |>
      factor(levels = c("Y", "N")),
    pos_time_point_flag = if_else(is.na(pos_time_point_flag), "N", " ") |>
      factor(levels = c(" ", "N"))
  )

# final output needs only these columns
adishum <- adishum |>
  select(
    USUBJID,
    all_of(trtvar), # TRT01A
    colspan_trt, # column split
    adatri_b_flag,
    pos_time_point_flag,
    imevfl_pos,
    AVISIT_ATPT
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

  # remove Combined from Placebo spanning header
  rm_combo_from_placebo <- cond_rm_facets(
    facets = "Combined",
    ancestor_pos = NA,
    value = "Y",
    split = "colspan_trt"
  )

  mysplit <- make_split_fun(post = list(add_combo, rm_combo_from_placebo))
}

# colspan map built from adsl, not adishum
colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = "Y",
  active_grp_span_lbl = study_agent |>
    stringi::stri_trans_totitle(),
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

# remove control group from map if not needed
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
  append_topleft(c("Visit")) |>
  # first level header: Active Study Agent vs Placebo
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(
      map = colspan_trt_map
    )
  ) |>
  # second level header: individual treatment groups + Combined
  split_cols_by(
    trtvar,
    split_fun = mysplit
  ) |>
  # Section 2 --- Subjects positive for treatment-emergent antibodies ------------
  analyze(
    "adatri_b_flag",
    afun = a_freq_j,
    show_labels = "hidden",
    section_div = " ",
    extra_args = list(
      label = sprintf(
        "Subjects positive for treatment-emergent antibodies to %s~[super a]",
        study_agent
      ),
      val = "Y",
      .stats = "count_unique"
    )
  ) |>
  # Section 3 --- Time points: n/N(%) per visit ------------
  split_rows_by(
    "AVISIT_ATPT",
    child_labels = "hidden"
  ) |>
  split_rows_by(
    "imevfl_pos",
    split_fun = keep_split_levels("Y"),
    label_pos = "hidden",
    child_labels = "hidden"
  ) |>
  analyze(
    "pos_time_point_flag",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      drop_levels = FALSE,
      val = " ",
      denom = "n_df",
      .stats = "count_unique_denom_fraction"
    )
  )

# Section --- Create Table Shell ------------

result <- build_table(lyt, adishum, alt_counts_df = adsl, round_type = "sas")

################################################################################
# Post-Processing:
################################################################################

# Section --- Move visit labels onto data rows ------------
for (v in levels(adishum$AVISIT_ATPT)) {
  label_at_path(
    result,
    path = c("AVISIT_ATPT", v, "imevfl_pos", "Y", "pos_time_point_flag", "count_unique_denom_fraction. ")
  ) <- v
}

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table:
################################################################################


colwidth <- c(64, 23, 23, 25)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "landscape")
