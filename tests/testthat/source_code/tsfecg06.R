library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)
library(haven)

################################################################################
# Define script level parameters:
################################################################################

################################################################################
# - Define output ID and file location
# - Define treatment variable used (default=TRT01A)
# - Define population flag used (default=SAFFL)
# - Define control group label
# - PARAMCD=INTP: ECG Overall Interpretation parameter
# - AVALC categories: Abnormal, CS / Abnormal, NCS / Normal
# - Choose whether or not you want to present a combined active treatment column (default=TRUE)
# - Records selected: ANL02FL="Y" and (ABLFL="Y" or APOBLFL="Y")
################################################################################

tblid <- "TSFECG06"
fileid <- write_path(opath, tblid)
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

popfl <- "SAFFL"
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"

# Add Active Study Agent Combined column?
combined_colspan_trt <- TRUE

# ECG Overall Interpretation parameter
selparamcd <- "INTP"

# AVALC levels for ECG interpretation (order as shown in mock)
avalc_levels <- c("Abnormal, CS", "Abnormal, NCS", "Normal")

# All visits of interest (Baseline + post-baseline timepoints)
selvisit <- c(
  "Baseline",
  "Month 1",
  "Month 3",
  "Month 6",
  "Month 9",
  "Month 12",
  "Month 15",
  "Month 18",
  "Month 24"
)

if (combined_colspan_trt == TRUE) {
  # Set up levels and label for the required combined columns
  add_combo <- add_combo_facet(
    "Combined",
    label = "Combined",
    levels = c("Xanomeline High Dose", "Xanomeline Low Dose")
  )

  # choose if any facets need to be removed - e.g remove the combined column for placebo
  rm_combo_from_placebo <- cond_rm_facets(
    facets = "Combined",
    ancestor_pos = NA,
    value = " ",
    split = "colspan_trt"
  )

  mysplit <- make_split_fun(post = list(add_combo, rm_combo_from_placebo))
}

################################################################################
# Process Data:
################################################################################

adsl <- adsl_jnj |>
  select(STUDYID, USUBJID, all_of(c(popfl, trtvar))) |>
  filter(
    toupper(.data[[popfl]]) == "Y",
    !is.na(.data[[trtvar]])
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
  ) |>
  create_colspan_var(
    non_active_grp = ctrl_grp,
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = trtvar
  )

adeg <- adeg_jnj

### available  Interpretation parameters in study
selparamcd <- intersect(selparamcd, unique(adeg$PARAMCD))

adeg <- adeg |>
  select(
    STUDYID,
    USUBJID,
    all_of(c(popfl, trtvar)),
    AVISITN,
    AVISIT,
    PARAMCD,
    PARAM,
    PARAMN,
    AVALC,
    starts_with("ANL"),
    ABLFL,
    APOBLFL
  ) |>
  filter(
    !is.na(USUBJID),
    .data[[popfl]] == "Y",
    !is.na(.data[[trtvar]]),
    PARAMCD %in% selparamcd,
    AVISIT %in% selvisit
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

selparamcd <- as.character(
  adeg |> arrange(PARAMN) |> pull(PARAMCD) |> unique()
)

adeg <- adeg |>
  mutate(
    ABLFL := factor(ifelse(!is.na(ABLFL) & ABLFL == "Y", "Y", "N")),
    APOBLFL := factor(ifelse(!is.na(APOBLFL) & APOBLFL == "Y", "Y", "N")),
    PARAMCD := factor(PARAMCD, levels = selparamcd),
    PARAM := factor(PARAM),
    # Baseline records: label AVISIT as "Baseline" using ABLFL flag
    AVISIT = factor(
      ifelse(ABLFL == "Y" & !is.na(ABLFL), "Baseline", as.character(AVISIT)),
      levels = unique(.data[["AVISIT"]])[order(unique(.data[["AVISITN"]]))]
    ),
    # Apply ordered levels to AVALC for consistent display
    AVALC := factor(
      dplyr::case_when(
        toupper(AVALC) == "ABNORMAL, CS" ~ "Abnormal, CS",
        toupper(AVALC) == "ABNORMAL, NCS" ~ "Abnormal, NCS",
        toupper(AVALC) == "NORMAL" ~ "Normal",
        TRUE ~ NA_character_
      ),
      levels = avalc_levels
    )
  )

selvisit <- unique(as.character(adeg$AVISIT[adeg$AVISIT %in% selvisit]))

adeg <- inner_join(adsl, adeg, by = c("STUDYID", "USUBJID", popfl, trtvar))

# Filter to records of interest: Baseline (ABLFL=Y) and post-baseline (APOBLFL=Y),
# both requiring ANL02FL=Y, restricted to selected visits
adeg <- adeg |>
  filter(ANL02FL == "Y" & (ABLFL == "Y" | APOBLFL == "Y")) |>
  filter(AVISIT %in% selvisit) |>
  mutate(
    AVISIT = factor(as.character(AVISIT), levels = selvisit)
  )

################################################################################
# Define layout and build table:
################################################################################

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)


lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx",
  top_level_section_div = " "
) |>
  append_topleft("Study Visit") |>
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  )

if (combined_colspan_trt == TRUE) {
  lyt <- lyt |>
    split_cols_by(trtvar, split_fun = mysplit)
} else {
  lyt <- lyt |>
    split_cols_by(trtvar)
}

lyt <- lyt |>
  # ── Visit split: Baseline + post-baseline timepoints ────────────────────────
  split_rows_by(
    "AVISIT",
    split_fun = keep_split_levels(selvisit),
    label_pos = "topleft",
    split_label = "  Overall Interpretation, n (%)",
    section_div = " "
  ) |>
  # N count per visit (subjects with a record at this timepoint)
  analyze(
    "AVALC",
    afun = a_freq_j,
    extra_args = list(
      .stats = "n_df",
      denom = "N_col"
    ),
    show_labels = "hidden",
    indent_mod = 0L
  ) |>
  # AVALC category counts: Abnormal CS, Abnormal NCS, Normal — n (%)
  analyze(
    "AVALC",
    afun = a_freq_j,
    extra_args = list(
      .stats = "count_unique_fraction",
      denom = "n_df"
    ),
    table_names = "avalc_freq",
    show_labels = "hidden",
    indent_mod = 1L
  )

result <- build_table(lyt, adeg, alt_counts_df = adsl, round_type = "sas")


################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table
################################################################################


colwidth <- c(30, 21, 21, 21, 21)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "landscape")
