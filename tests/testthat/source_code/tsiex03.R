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
# - Define levels which will control ordering for AVALCAT2 (total number of administrations categories)
#     in the table. Set as a character vector to control ordering.
# - Set to TRUE to include Total Number of Administrations section (default=TRUE)
# - Set to TRUE to include AVALCAT2 (total number of administrations categories) rows (default=TRUE)
# - Set to TRUE to include CRIT variables for total number of administrations (default=TRUE)
# - Define control group (default=Placebo)
# - Set to TRUE to include difference in mean between treatment groups row (default=TRUE)
# - Set to TRUE to use ANCOVA approach for group comparison, FALSE for 2-sample t-test (default=TRUE)
# - Set to TRUE to include Interquartile range row in summary statistics blocks (default=TRUE)
# - Set to TRUE to include Total treatment (subject years) row (default=TRUE)
# - Define parameter code needed for duration of treatment
# - Conversion to get the unit of PARAMCD selected for treatment
#     (i.e 1 if original PARAMCD unit is days, 30.4375 if original PARAMCD unit is in months)
# - Define levels which will control ordering for AVALCAT1 (duration of treatment categories)
#     in the table. Set as a character vector to control ordering.
# - Choose whether or not you want to present a combined active treatment column (default=TRUE)
# - Define how to create combined treatment columns (if required)
################################################################################

tblid <- "TSIEX03"
fileid <- write_path(opath, tblid)
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)


trtvar <- "TRT01A"
popfl <- "SAFFL"

# Levels for AVALCAT2 (total number of administrations categories). Set as a character vector to control ordering.
totadminlevels <- c("1 to <10", "10 to <20", ">=20")

# Set to TRUE to include Total Number of Administrations section (default=TRUE)
show_totadmin <- TRUE
# Set to TRUE to include AVALCAT2 (total number of administrations categories) rows (default=TRUE)
show_totadmin_avalcat <- TRUE
# Set to TRUE to include CRIT variables for total number of administrations (default=TRUE)
show_totadmin_crit <- TRUE

ctrl_grp <- "Placebo"

# Set to TRUE to include difference in mean between treatment groups row (default=TRUE)
comp_btw_group <- TRUE
# Set to TRUE to use ANCOVA approach for group comparison, FALSE for 2-sample t-test (default=TRUE)
ancova <- TRUE

# Set to TRUE to include Interquartile range row in summary statistics blocks (default=TRUE)
show_iqr <- TRUE

# Set to TRUE to include Total treatment (subject years) row (default=TRUE)
show_subj_years <- TRUE

trtdur <- "TRTDURM"

# Levels for AVALCAT1 (duration of treatment categories). Set as a character vector to control ordering.
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
  select(USUBJID, all_of(trtvar), all_of(popfl))

# If AVISIT is not present in ADEXSUM than create 'Overall' as the Visit, which is used
# for the filtering AVISIT records if it does exist
# Duration of treatment parameter
adexsum1 <- adexsum_jnj |>
  mutate(VISIT = if (exists("AVISIT")) AVISIT else "Overall") |>
  filter((PARAMCD == trtdur | PARAMCD == "TRTDURD") & !is.na(AVAL) & VISIT == "Overall") |>
  mutate(
    AVAL1 = ifelse(PARAMCD == trtdur, AVAL, NA),
    # Added AVAL_yr for subject years derivation from TRTDURD, /365.25 converts from days to years
    AVAL_yr = ifelse(PARAMCD == "TRTDURD", AVAL / 365.25, NA),
    CRIT0FL = "Y",
    CRIT0 = as.factor("Any duration (at least 1 dose)")
  ) |>
  select(STUDYID, USUBJID, PARAMCD, PARAM, AVAL1, AVAL_yr, AVALCAT1, starts_with("CRIT"))

# Total Number of Administrations
adexsum2 <- adexsum_jnj |>
  mutate(VISIT = if (exists("AVISIT")) AVISIT else "Overall") |>
  filter(PARAMCD == "TNUMDOS" & !is.na(AVAL) & VISIT == "Overall") |>
  mutate(AVAL2 = AVAL, AVALCAT2 = AVALCAT1) |>
  select(STUDYID, USUBJID, PARAMCD, PARAM, AVAL2, AVALCAT2, starts_with("CRIT")) |>
  rename_with(~ paste0(., "_NDOS"), starts_with("CRIT"))

# Cumulative dose
adexsum3 <- adexsum_jnj |>
  mutate(VISIT = if (exists("AVISIT")) AVISIT else "Overall") |>
  filter(PARAMCD == "CUMDOSE" & !is.na(AVAL) & VISIT == "Overall") |>
  mutate(AVAL3 = AVAL) |>
  select(STUDYID, USUBJID, PARAMCD, PARAM, AVAL3)

adexsum <- bind_rows(adexsum1, adexsum2, adexsum3) |>
  select(STUDYID, USUBJID, PARAMCD, PARAM, AVAL1, AVAL2, AVAL3, AVAL_yr, AVALCAT1, AVALCAT2, starts_with("CRIT"))

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

# Work out how many CRITy vars we have left
excritvars <- ex |>
  select(num_range("CRIT", 1:99))

countcritvars <- length(names(excritvars))

for (i in 1:countcritvars) {
  variable_name <- paste0("CRIT", i, "FL")
  ex[[variable_name]] <- droplevels(ex[[variable_name]])
}

# drop unwanted levels from AVALCAT1 and assign levels from specified section at the top of script
ex$AVALCAT1 <- droplevels(ex$AVALCAT1)
ex$AVALCAT1 <- factor(ex$AVALCAT1, levels = trtdurlevels)

# Drop unwanted levels for all CRITy variables you have remaining in ex and also for AVALCAT2
ex2critvars <- ex |>
  select(num_range("CRIT", 1:99, suffix = "_NDOS"))

count2critvars <- length(names(ex2critvars))

critlbls <- list()
for (i in 1:count2critvars) {
  variable_name <- paste0("CRIT", i, "_NDOS")
  ex[[variable_name]] <- droplevels(ex[[variable_name]])
  critlbls[[i]] <- unique(as.character(ex[[variable_name]][
    !is.na(ex[[variable_name]])
  ]))
}

# drop unwanted levels from AVALCAT1 and assign levels from specified section at the top of script
ex$AVALCAT2 <- droplevels(ex$AVALCAT2)
ex$AVALCAT2 <- factor(ex$AVALCAT2, levels = totadminlevels)

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = "Placebo",
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)

###################################################################
# Create label_map to be utilized in the Criterion variables (CRIT)
# This is needed as the CRIT variables are not labeled in the dataset
# for Duration of Treatment (TRTDURM) parameter
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

.stats_all <- c("n", "mean_sd", "median", "range", "sum", "quantiles", "lsmean_diff_with_ci", "diff_means_est_ci")

dp <- 1
.formats_all <- junco:::fmt_spec_single_d(
  d = dp,
  stats_in = .stats_all,
  fmt_d_def = junco_def_d_all,
  fmt_d_in = NULL
)
fmt_details <- get_fmt_details(.formats_all, as_tibble = TRUE)

# some parameters (Total dosing days, and daily dose) are reported in days or mg - these follow format rules with d = 0
.formats_all0 <- junco:::fmt_spec_single_d(
  d = 0,
  stats_in = .stats_all,
  fmt_d_def = junco_def_d_all,
  fmt_d_in = NULL
)

# labels
aval1_stats <- c("n", "mean_sd", "median", "range", if (show_iqr) "quantiles")

aval1_labels <- c(n = "N", range = "Min, max", quantiles = "Interquartile range")
aval1_indent_mods <- c(
  n = 0L,
  mean_sd = 1L,
  median = 1L,
  range = 1L,
  quantiles = 1L,
  sum = 1L,
  "lsmean_diff_with_ci" = 1L,
  "diff_means_est_ci" = 1L
)

################################################################################
# Define layout and build table:
################################################################################

extra_args_rr <- list(.stats = c("count_unique_fraction"), riskdiff = FALSE)

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

# Duration of Treatment
lyt <- lyt |>
  analyze(
    "AVAL1",
    afun = a_summary,
    var_labels = sprintf("Duration of treatment~[super a], (%s)", get_unit(trtdur)),
    show_labels = "visible",
    indent_mod = 0L,
    extra_args = list(
      ref_path = ref_path,
      .stats = aval1_stats,
      .formats = .formats_all,
      .labels = aval1_labels,
      .indent_mods = aval1_indent_mods
    )
  )

if (show_subj_years) {
  lyt <- lyt |>
    analyze(
      "AVAL_yr",
      afun = a_summary,
      var_labels = sprintf("Duration of treatment~[super a], (%s)", get_unit(trtdur)),
      show_labels = "hidden",
      indent_mod = 2L,
      extra_args = list(
        ref_path = ref_path,
        .stats = c("sum"),
        .formats = .formats_all,
        .labels = c(sum = "Total treatment (subject years)")
      )
    )
}

if (comp_btw_group) {
  diff_lbl <- "Difference in means [Study Treatment] vs. [Placebo] (95% CI)"
  diff_fmt <- jjcsformat_xx("xx.xx (xx.xx, xx.xx)")
  if (ancova) {
    message("difference between groups using ancova approach")
    lyt <- lyt |>
      analyze(
        vars = "AVAL1",
        table_names = "AVAL_diff",
        afun = a_summarize_ancova_j,
        show_labels = "hidden",
        indent_mod = 2L,
        extra_args = list(
          ref_path = ref_path,
          variables = list(arm = trtvar, covariates = NULL),
          conf_level = 0.95,
          weights_combo = "proportional",
          .stats = "lsmean_diff_with_ci",
          .formats = .formats_all,
          .labels = c(lsmean_diff_with_ci = diff_lbl)
        ),
        section_div = " "
      )
  } else {
    message("difference between groups using 2 sample t-test approach")
    lyt <- lyt |>
      analyze(
        vars = "AVAL1",
        table_names = "AVAL_diff_desc",
        afun = a_diff_means,
        show_labels = "hidden",
        indent_mod = 2L,
        extra_args = list(
          ref_path = ref_path,
          conf_level = 0.95,
          .stats = "diff_means_est_ci",
          .formats = .formats_all,
          .labels = c(diff_means_est_ci = diff_lbl)
        ),
        section_div = " "
      )
  }
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

# Total Number of Administrations
if (show_totadmin) {
  lyt <- lyt |>
    analyze(
      "AVAL2",
      nested = FALSE,
      var_labels = "Total number of administrations",
      show_labels = "visible",
      afun = a_summary,
      extra_args = list(
        .stats = aval1_stats,
        .formats = .formats_all0,
        .labels = aval1_labels,
        .indent_mods = aval1_indent_mods
      )
    )

  if (show_totadmin_avalcat) {
    lyt <- lyt |>
      analyze(
        "AVALCAT2",
        nested = FALSE,
        afun = a_freq_j,
        var_labels = "Total number of administrations, n (%)",
        extra_args = extra_args_rr,
        show_labels = "visible",
        indent_mod = 1L
      )
  }

  if (show_totadmin_crit) {
    lyt <- lyt |>
      analyze(
        "CRIT1FL_NDOS",
        afun = a_freq_j,
        nested = FALSE,
        show_labels = "visible",
        var_labels = "Total number of administrations~[super b], n (%)",
        extra_args = list(
          val = "Y",
          label = critlbls[[1]],
          denom = "n_df",
          .stats = c("count_unique_fraction")
        ),
        indent_mod = 1L
      )

    # Add in analyze for all remaining CRIT variables contained in ex
    for (i in 2:count2critvars) {
      lyt <- lyt |>
        analyze(
          paste0("CRIT", i, "FL_NDOS"),
          afun = a_freq_j,
          extra_args = list(
            val = "Y",
            label = critlbls[[i]],
            denom = "n_df",
            .stats = c("count_unique_fraction")
          ),
          indent_mod = 2L,
          show_labels = "hidden"
        )
    }
  }
}
# end show_totadmin

# Cumulative Dose [unit]
lyt <- lyt |>
  analyze(
    "AVAL3",
    nested = FALSE,
    var_labels = sprintf("Cumulative dose (%s)", get_unit("CUMDOSE")),
    show_labels = "visible",
    afun = a_summary,
    extra_args = list(
      .stats = aval1_stats,
      .formats = .formats_all0,
      .labels = aval1_labels,
      .indent_mods = aval1_indent_mods
    )
  ) |>
  append_topleft("Parameter")

result <- build_table(lyt, ex, alt_counts_df = adsl, round_type = "sas")

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table
################################################################################

colwidth <- c(64, 34, 35, 32, 31)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "landscape")
