library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

################################################################################
# - Define output ID and file location
# - Define treatment variable used (default=TRT01A)
# - Define population flag used (default=SAFFL)
# - Choose whether or not you want to present a combined active treatment column (default=TRUE)
# - Define how to create combined treatment columns (if required)
################################################################################

tblid <- "TSIEX03b"
fileid <- write_path(opath, tblid)
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


trtvar <- "TRT01A"
popfl <- "SAFFL"
combined_colspan_trt <- TRUE
ctrl_grp <- "Placebo"

# dose_dly will be TRUE if study collected dose delay by default it is FALSE
dose_dly <- TRUE

# trt_period will be TRUE if APERIOD details available by default it is FALSE
trt_period <- TRUE

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

# Read in required data
adsl <- adsl_jnj |>
  filter(!!rlang::sym(popfl) == "Y") |>
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
  select(STUDYID, USUBJID, all_of(trtvar), all_of(popfl))

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

core_ex_vars <- c(
  "USUBJID",
  "AACTPR",
  "AACTDU",
  "AADJ",
  "AADJP",
  "AVISIT",
  "AVISITN"
)

# Conditionally include dose delay variables if dose_dly is TRUE
dose_dly_vars <- if (isTRUE(dose_dly)) c("ADOSDLY", "ARSDOSD") else NULL

# Conditionally include treatment period variables if trt_period is TRUE
period_vars <- if (isTRUE(trt_period)) c("APERIOD", "APERIODC") else NULL

# Read adex, exclude unscheduled visits, select required variables
adex1 <- adex_jnj |>
  filter(!grepl("UNSCHEDULED", AVISIT, ignore.case = TRUE)) |>
  select(all_of(core_ex_vars), all_of(dose_dly_vars), all_of(period_vars))

# Convert AVISIT to sentence case and apply levels sorted by AVISITN
avisit_levs <- adex1 |>
  distinct(AVISIT, AVISITN) |>
  arrange(AVISITN) |>
  pull(AVISIT) |>
  stringr::str_to_sentence()
adex1$AVISIT <- stringr::str_to_sentence(adex1$AVISIT)
adex1$AVISIT <- factor(adex1$AVISIT, levels = avisit_levs)

adex1a <- adex1 |>
  mutate(
    AACTx = AACTPR,
    AADJx = AADJP,
    XXX = "Action taken prior to [injection/infusion]"
  )

adex1b <- adex1 |>
  mutate(
    AACTx = AACTDU,
    AADJx = AADJ,
    XXX = "Action taken during [injection/infusion]"
  )

if (isTRUE(dose_dly)) {
  adex1c <- adex1 |>
    filter(ADOSDLY == "Y") |>
    mutate(
      AACTx = ADOSDLY,
      AADJx = ARSDOSD,
      XXX = "Dose delay"
    )

  adex_all <- bind_rows(adex1a, adex1b, adex1c)
} else {
  adex_all <- bind_rows(adex1a, adex1b)
}

any_label <- if (isTRUE(dose_dly)) {
  "Any action taken or dose delay of study treatment~[super a]"
} else {
  "Any action taken of study treatment~[super a]"
}
adex_any <- adex_all |> mutate(ANY = any_label)

adex <- bind_rows(adex_any, adex_all)

# Adding factor level for categories
adex$ANY <- factor(adex$ANY, levels = unique(adex$ANY))

# Convert ARSDOSD to sentence case
if (isTRUE(dose_dly)) {
  adex$ARSDOSD <- factor(
    stringr::str_to_sentence(as.character(adex$ARSDOSD)),
    levels = stringr::str_to_sentence(levels(adex$ARSDOSD))
  )
}

# Re-apply APERIOD/APERIODC factor levels after bind_rows (bind drops levels)
if (isTRUE(trt_period)) {
  adex$APERIOD <- factor(adex$APERIOD, levels = c("1", "2"))
  adex$APERIODC <- factor(adex$APERIODC, levels = c("Period 1", "Period 2"))
}

xxx_levels <- c(
  "Action taken prior to [injection/infusion]",
  "Dose delay",
  "Action taken during [injection/infusion]"
)

# control levels of new variables AACTx and AADJx
aact_levels <- c(
  "DOSE REDUCED COMPARED TO PRIOR INFUSION",
  "INFUSION DELAYED WITHIN THE CYCLE",
  "INFUSION RATE DECREASED COMPARED TO PRIOR INFUSION",
  "INFUSION SKIPPED (AND NOT MADE UP)",
  "STUDY DRUG PERMANENTLY DISCONTINUED",
  "INFUSION ABORTED",
  "INFUSION INTERRUPTED",
  "INFUSION RATE INCREASED"
)

aact_labels <- c(
  "Dose reduced",
  "Infusion delayed",
  "Infusion rate decreased",
  "Infusion skipped",
  "Study agent permanently discontinued",
  "Infusion aborted",
  "Infusion interrupted",
  "Infusion rate increased"
)

adex$AACTx <- factor(
  as.character(adex$AACTx),
  levels = aact_levels,
  labels = aact_labels
)

adex$AADJx <- factor(
  stringr::str_to_sentence(as.character(adex$AADJx)),
  levels = stringr::str_to_sentence(levels(c(adex$AADJ, adex$AADJP)))
)

### Note the following levels are not used on the table, not sure if this is OK ---
### this is out of scope for template script
### how standard is this table and the levels of the variables on the current dataset + standard ADaM???
setdiff(levels(adex$AACTPR), aact_levels)
setdiff(levels(adex$AACTDU), aact_levels)

# join data together
ex <- adex |> inner_join(adsl, by = c("USUBJID"))

################################################################################
# Define layout and build table:
################################################################################
# Build colspan mapping for active vs control treatment column headers
colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)
# Reference path used for column count denominators (control group anchor)
ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)

# extra_args1: frequency args for treatment period analysis (denominator by APERIOD)
extra_args1 <- list(
  .stats = "count_unique_fraction",
  denom = "n_parentdf",
  denom_by = "APERIOD",
  drop_levels = TRUE
)
# extra_args2: frequency args for time point analysis (denominator by AVISIT)
extra_args2 <- list(
  .stats = "count_unique_fraction",
  denom = "n_parentdf",
  denom_by = "AVISIT",
  drop_levels = TRUE
)

# --- Column structure ---
lyt0 <- rtables::basic_table(
  top_level_section_div = " ",
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) |>
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  )

if (combined_colspan_trt == TRUE) {
  lyt <- lyt0 |> split_cols_by(trtvar, split_fun = mysplit)
} else {
  lyt <- lyt0 |> split_cols_by(trtvar)
}

# --- Section 1: Overall summary (ANY) ---
lyt <- lyt |>
  split_rows_by(
    "ANY",
    split_label = "Time Point",
    split_fun = drop_split_levels,
    label_pos = "topleft",
    indent_mod = 0L,
    section_div = " "
  ) |>
  split_rows_by("AACTx", split_fun = drop_split_levels, section_div = " ") |>
  summarize_row_groups("AACTx", cfun = a_freq_j, extra_args = list(.stats = "count_unique_fraction")) |>
  analyze("AADJx", afun = a_freq_j, extra_args = list(.stats = "count_unique_fraction"))
if (isTRUE(dose_dly)) {
  lyt <- lyt |>
    split_rows_by("XXX", split_fun = keep_split_levels(c("Dose delay")), indent_mod = 1L) |>
    summarize_row_groups("XXX", cfun = a_freq_j, extra_args = list(.stats = "count_unique_fraction")) |>
    analyze("ARSDOSD", afun = a_freq_j, extra_args = list(.stats = "count_unique_fraction"))
}

# --- Section 2: Treatment Period breakdown (only when trt_period = TRUE) ---
if (isTRUE(trt_period)) {
  lyt <- lyt |>
    split_rows_by(
      "APERIOD",
      labels_var = "APERIODC",
      split_fun = keep_split_levels(c("1", "2")),
      child_labels = "visible",
      indent_mod = 0L,
      section_div = " "
    )
  if (isTRUE(dose_dly)) {
    lyt <- lyt |>
      split_rows_by("XXX", split_fun = keep_split_levels(xxx_levels), section_div = " ") |>
      split_rows_by("AACTx", split_fun = drop_split_levels, section_div = " ") |>
      summarize_row_groups("AACTx", cfun = a_freq_j, extra_args = extra_args1) |>
      analyze("AADJx", afun = a_freq_j, extra_args = extra_args1)
  } else {
    lyt <- lyt |>
      split_rows_by(
        "XXX",
        split_fun = keep_split_levels(c(
          "Action taken prior to [injection/infusion]",
          "Action taken during [injection/infusion]"
        )),
        section_div = " "
      ) |>
      split_rows_by("AACTx", split_fun = drop_split_levels, section_div = " ") |>
      summarize_row_groups("AACTx", cfun = a_freq_j, extra_args = extra_args1) |>
      analyze("AADJx", afun = a_freq_j, extra_args = extra_args1)
  }
}

# --- Section 3: Time Point (AVISIT) breakdown ---
lyt <- lyt |>
  split_rows_by(
    "AVISIT",
    split_fun = drop_split_levels,
    child_labels = "visible",
    indent_mod = 0L,
    section_div = " "
  )
if (isTRUE(dose_dly)) {
  lyt <- lyt |>
    split_rows_by("XXX", split_fun = keep_split_levels(xxx_levels), section_div = " ") |>
    summarize_row_groups("XXX", cfun = a_freq_j, extra_args = list(.stats = "n_df")) |>
    split_rows_by("AACTx", split_fun = drop_split_levels, section_div = " ") |>
    summarize_row_groups("AACTx", cfun = a_freq_j, extra_args = extra_args2) |>
    analyze("AADJx", afun = a_freq_j, extra_args = extra_args2) |>
    append_topleft("  Action Taken or Dose Delay") |>
    append_topleft("    Reason, n (%)")
} else {
  lyt <- lyt |>
    split_rows_by(
      "XXX",
      split_fun = keep_split_levels(c(
        "Action taken prior to [injection/infusion]",
        "Action taken during [injection/infusion]"
      )),
      section_div = " "
    ) |>
    summarize_row_groups("XXX", cfun = a_freq_j, extra_args = list(.stats = "n_df")) |>
    split_rows_by("AACTx", split_fun = drop_split_levels, section_div = " ") |>
    summarize_row_groups("AACTx", cfun = a_freq_j, extra_args = extra_args2) |>
    analyze("AADJx", afun = a_freq_j, extra_args = extra_args2) |>
    append_topleft("  Action Taken") |>
    append_topleft("    Reason, n (%)")
}

result <- build_table(lyt, ex, alt_counts_df = adsl, round_type = "sas")

################################################################################
# Prune table to remove all the rows with 0 counts or no data to report across all sections
################################################################################

result <- prune_table(result, prune_func = prune_zeros_only)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table
################################################################################


colwidth <- c(64, 21, 21, 21, 21)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, label_width_ins = 2.4, orientation = "portrait")
