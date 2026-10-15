library(envsetup)
library(haven)
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
# - Define subgroup variable for subgroup analyses (default=NULL, no subgroup)
# - Define parameter code needed for duration of treatment
# - Conversion to get the unit of PARAMCD selected for treatment
#     (i.e 1 if original PARAMCD unit is days, 30.4375 if original PARAMCD unit is in months)
# - Define levels which will control ordering for AVALCAT1 in the table
# - Choose whether or not you want to present a combined active treatment column (default=TRUE)
# - Define how to create combined treatment columns (if required)
# - Choose whether or not to include Interquartile range row (default=TRUE)
# - Choose whether or not to include Total treatment (subject years) row (default=TRUE)
################################################################################

tblid <- "TSIEX01"
fileid <- write_path(opath, tblid)
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


trtvar <- "TRT01A"
popfl <- "SAFFL"
ctrl_grp <- "Placebo"

# Subgroup variable used for subgroup analyses. Set to NULL to suppress subgroup splits,
# or set to a character string with the variable name (e.g. "SEX", "AGEGR1").
subgrpvar <- "AGEGR1"

# Set to TRUE to include Interquartile range row in summary statistics blocks (default=TRUE)
show_iqr <- TRUE

# Set to TRUE to include Total treatment (subject years) row (default=TRUE)
show_subj_years <- TRUE

trtdur <- "TRTDURM"

# Levels for AVALCAT7 (treatment duration categories, derived from PARAMCD=trtdur).
# Set as a character vector to control ordering of rows in the duration category table.
trtdurlevels <- c(
  "0 to <3 months",
  "3 to <6 months",
  "6 to <9 months",
  "9 to <12 months",
  "12 to <15 months",
  "15 to <18 months",
  "18 to <21 months",
  "21 to <24 months",
  "24 to <27 months",
  "27 to <30 months",
  "30 to <33 months",
  "33 to <36 months",
  "36 to <39 months"
)

combined_colspan_trt <- TRUE

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
  select(STUDYID, USUBJID, all_of(trtvar), all_of(popfl), all_of(subgrpvar))

# If AVISIT is not present in ADEXSUM than create 'Overall' as the Visit, which is used
# for the filtering AVISIT records if it does exist
adexsum <- adexsum_jnj |>
  mutate(VISIT = if (exists("AVISIT")) AVISIT else "Overall") |>
  filter((PARAMCD == trtdur | PARAMCD == "TRTDURD") & !is.na(AVAL) & VISIT == "Overall") |>
  mutate(
    CRIT0FL = "Y",
    CRIT0 = as.factor("Any duration (at least 1 dose)")
  ) |>
  select(USUBJID, PARAMCD, PARAM, AVAL, AVALCAT1, starts_with("CRIT")) |>
  # Added AVAL_yr for subject years derivation from TRTDURD, /365.25 converts from days to years
  mutate(AVAL_yr = ifelse(PARAMCD == "TRTDURD", AVAL / 365.25, NA), AVAL_anly = ifelse(PARAMCD == trtdur, AVAL, NA))

adexsum$CRIT0FL <- factor(adexsum$CRIT0FL, levels = c("Y", "N"))

# Extract unit from PARAM: captures content of first parentheses e.g. "Duration of Treatment (months)" -> "months"
# If unit is present in middle parentheses (not last), e.g. "Average daily dose ([unit]/day) (including days off treatment)" -> "[unit]/day"
unit_map <- adexsum |>
  distinct(PARAMCD, PARAM) |>
  filter(!is.na(PARAM)) |>
  mutate(
    unit = ifelse(
      grepl("\\(.+\\)", PARAM),
      gsub("^[^(]*\\(([^)]+)\\).*$", "\\1", PARAM),
      ""
    )
  ) |>
  select(PARAMCD, unit) |>
  tibble::deframe()

get_unit <- function(paramcd) unit_map[[paramcd]] %||% ""

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == "Placebo", " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

# join data together
ex <- adexsum |> inner_join(adsl, by = c("USUBJID"))

# Keep only columns with some data in which will remove any unwanted CRITy variables
ex <- ex[, colSums(is.na(ex)) < nrow(ex)]

# Work out how many CRITy vars (ignoring CRIT0 we created) we have left
excritvars <- ex |>
  select(num_range("CRIT", 1:99))

countcritvars <- length(names(excritvars))

# Drop unwanted levels for all CRITy variables you have remaining in ex and also for AVALCAT1

for (i in 1:countcritvars) {
  variable_name <- paste0("CRIT", i)
  ex[[variable_name]] <- droplevels(ex[[variable_name]])
}

# drop unwanted levels from AVALCAT1 and assign levels from specified section at the top of script
ex$AVALCAT1 <- droplevels(ex$AVALCAT1)
ex$AVALCAT1 <- factor(ex$AVALCAT1, levels = trtdurlevels)

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)
ref_path <- c("colspan_trt", "", trtvar, ctrl_grp)

###################################################################
# Create label_map to be utilized in the Criterion variables (CRIT)
###################################################################

ex_lblmap <- ex |>
  select(-CRIT0, -CRIT0FL) |>
  select(starts_with("CRIT")) |>
  tidyr::pivot_longer(
    cols = starts_with("CRIT") & ends_with("FL"),
    names_to = "var",
    values_to = "value"
  ) |>
  filter(value == "Y") |>
  distinct() |>
  tidyr::pivot_longer(
    cols = starts_with("CRIT"),
    names_to = "lbl_var",
    values_to = "label"
  ) |>
  filter(var == paste0(lbl_var, "FL")) |>
  select(var, value, label) |>
  mutate(label = as.character(label)) |>
  arrange(var)

################################################################################
# Formats for decimal precision control
################################################################################

.stats_all <- c("mean_sd", "median", "range", "sum", "quantiles")
dp <- 1
.formats_all <- junco:::fmt_spec_single_d(
  d = dp,
  stats_in = .stats_all,
  fmt_d_def = junco_def_d_all,
  fmt_d_in = NULL
)
fmt_details <- get_fmt_details(.formats_all, as_tibble = TRUE)

################################################################################
# Define layout and build table:
################################################################################

extra_args_rr <- list(.stats = c("count_unique_fraction"), riskdiff = FALSE)

if (!is.null(subgrpvar)) {
  extra_args_rr <- append(extra_args_rr, list(denom = c("n_df"), denom_by = c(subgrpvar)))
}

lyt <- rtables::basic_table(
  top_level_section_div = " ",
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) |>
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

if (!is.null(subgrpvar)) {
  lyt <- lyt |>
    split_rows_by(subgrpvar, section_div = " ") |>
    summarize_row_groups(
      var = subgrpvar,
      cfun = a_freq_j,
      extra_args = list(.stats = "n_df", label_fstr = "Subgroup: %s")
    )
}

lyt <- lyt |>
  analyze(
    "AVAL_anly",
    afun = a_summary,
    var_labels = sprintf("Duration of treatment~[super a], (%s)", get_unit(trtdur)),
    show_labels = "visible",
    indent_mod = 0L,
    extra_args = list(
      ref_path = ref_path,
      .stats = c("mean_sd", "median", "range", if (show_iqr) "quantiles"),
      .formats = .formats_all,
      .labels = c(range = "Min, max", quantiles = "Interquartile range"),
      .indent_mods = c("mean_sd" = 0L, "median" = 0L, "range" = 0L, "quantiles" = 0L)
    )
  )

if (show_subj_years) {
  lyt <- lyt |>
    analyze(
      "AVAL_yr",
      afun = a_summary,
      var_labels = sprintf("Duration of treatment~[super a], (%s)", get_unit(trtdur)),
      show_labels = "hidden",
      indent_mod = 1L,
      extra_args = list(
        ref_path = ref_path,
        .stats = c("sum"),
        .formats = .formats_all,
        .labels = c(sum = "Total treatment (subject years)")
      ),
      section_div = " "
    )
}

lyt <- lyt |>
  analyze(
    "AVALCAT1",
    var_labels = "Duration of treatment, n (%)",
    afun = a_freq_j,
    extra_args = extra_args_rr,
    indent_mod = 1L,
    show_labels = "visible",
    section_div = " "
  )

# Add in analyze for all CRIT variables contained in ex

lyt <- lyt |>
  analyze(
    "CRIT1FL",
    var_labels = "Duration of treatment~[super b], n (%)",
    afun = a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(label_map = ex_lblmap, val = "Y")
    ),
    indent_mod = 1L,
    show_labels = "visible"
  )

if (countcritvars > 1) {
  critvars <- paste0("CRIT", 2:countcritvars, "FL")
  lyt <- lyt |>
    analyze(
      critvars,
      afun = a_freq_j,
      extra_args = append(
        extra_args_rr,
        list(label_map = ex_lblmap, val = "Y")
      ),
      indent_mod = 2L,
      show_labels = "hidden"
    )
}

lyt <- lyt |>
  append_topleft("Parameter")

result <- build_table(lyt, ex, alt_counts_df = adsl, round_type = "sas")

if (!is.null(subgrpvar)) {
  section_div_at_path(result, c("*", "@content", "n_df")) <- " "
}

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table
################################################################################

colwidth <- c(64, 34, 34, 34, 34)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "landscape")
