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
# - Define levels which will control ordering for AVALCAT variables in the table
# - Choose whether or not you want to present a combined active treatment column (default=TRUE)
# - Define how to create combined treatment columns (if required)
################################################################################

tblid <- "TSIEX02"
fileid <- write_path(opath, tblid)
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


trtvar <- "TRT01A"
popfl <- "SAFFL"
ctrl_grp <- "Placebo"

# Set to TRUE to include difference in mean between treatment groups row (default=TRUE)
comp_btw_group <- TRUE

# Set to TRUE to use ANCOVA approach for group comparison, FALSE for 2-sample t-test (default=TRUE)
ancova <- TRUE

# Set to TRUE to include Interquartile range row in summary statistics blocks (default=TRUE)
show_iqr <- TRUE

# Set to TRUE to include Total treatment (subject years) row (default=TRUE)
show_subj_years <- TRUE

# Duration of treatment category
trtdur <- "TRTDURM"

# Levels for AVAL1_C1 (treatment duration categories). Set as a character vector to control ordering.
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

# Levels for (dosing days categories). Set to NULL to derive from data,
# or pre-define as a character vector to control ordering.
dsdyslevels_c1 <- c("1 to <30 days", "30 to <60 days", "60 to <90 days", ">=90 days")

# Set to TRUE to include dosing days category (AVALCAT2 (renamed AVALCAT1 values of DOSEDAYS)) row (default=TRUE)
show_cat_dsdys <- TRUE

# Levels for modal and final dose categories (AVALCAT6, AVALCAT7). Set as a character vector to control ordering.
mddlevels <- c("20 mg", "30 mg", "40 mg")

# Set to TRUE to include Average daily dose (including days off treatment) block (MEANDDI) (default=TRUE)
show_meanddi <- TRUE

# Set to TRUE to include Average daily dose (excluding days off treatment) block (MEANDD) (default=TRUE)
show_meandd <- TRUE

# Set to TRUE to include Modal daily dose block (MODEDD) (default=TRUE)
show_modedd <- TRUE

# Set to TRUE to include modal daily dose category (AVALCAT6 (renamed AVALCAT1 values of MODEDD)) row (default=TRUE)
show_cat_modedd <- TRUE

# Set to TRUE to include Final daily dose block (FINDD) (default=TRUE)
show_findd <- TRUE

# Set to TRUE to include final daily dose category (AVALCAT7 (renamed AVALCAT1 values of FINDD)) row (default=TRUE)
show_cat_findd <- TRUE

combined_colspan_trt <- FALSE

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
adexsum <- adexsum_jnj |>
  mutate(VISIT = if (exists("AVISIT")) AVISIT else "Overall") |>
  filter(
    PARAMCD %in%
      c(
        trtdur,
        "TRTDURD",
        "DOSEDAYS",
        "CUMDOSE",
        if (show_meanddi) "MEANDDI",
        if (show_meandd) "MEANDD",
        if (show_modedd) "MODEDD",
        if (show_findd) "FINDD"
      ) &
      !is.na(AVAL) &
      VISIT == "Overall"
  ) |>
  mutate(
    AVAL1 = case_when(PARAMCD == trtdur ~ AVAL),
    AVAL1_C1 = case_when(PARAMCD == trtdur ~ AVALCAT1),
    AVAL2 = case_when(PARAMCD == "DOSEDAYS" ~ AVAL),
    AVALCAT2 = case_when(PARAMCD == "DOSEDAYS" ~ AVALCAT1),
    AVAL3 = case_when(PARAMCD == "CUMDOSE" ~ AVAL),
    AVAL4 = case_when(PARAMCD == "MEANDDI" ~ AVAL),
    AVAL5 = case_when(PARAMCD == "MEANDD" ~ AVAL),
    AVAL6 = case_when(PARAMCD == "MODEDD" ~ AVAL),
    AVALCAT6 = case_when(PARAMCD == "MODEDD" ~ AVALCAT1),
    AVAL7 = case_when(PARAMCD == "FINDD" ~ AVAL),
    AVALCAT7 = case_when(PARAMCD == "FINDD" ~ AVALCAT1),
    CRIT0FL = case_when(PARAMCD == trtdur ~ "Y"),
    CRIT0 = case_when(PARAMCD == trtdur ~ as.factor("Any duration (at least 1 dose)")),
    # Added AVAL_yr for subject years derivation from TRTDURD, /365.25 converts from days to years
    AVAL_yr = case_when(PARAMCD == "TRTDURD" ~ AVAL / 365.25)
  ) |>
  select(
    STUDYID,
    USUBJID,
    PARAM,
    PARAMCD,
    AVAL1,
    AVAL2,
    AVALCAT2,
    AVAL3,
    AVAL4,
    AVAL5,
    AVAL6,
    AVAL7,
    AVAL1_C1,
    AVALCAT6,
    AVALCAT7,
    AVAL_yr,
    starts_with("CRIT")
  )

# Extract unit from PARAM: captures content of last parentheses e.g. "Duration of Treatment (months)" -> "months"
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

# Work out how many CRITy vars (ignoring CRIT0 we created) we have left
excritvars <- ex |>
  select(num_range("CRIT", 1:99))

countcritvars <- length(names(excritvars))

# Drop unwanted levels for all CRITy variables you have remaining in ex and also for AVALCAT1

for (i in 1:countcritvars) {
  variable_name <- paste0("CRIT", i)
  ex[[variable_name]] <- droplevels(ex[[variable_name]])
}

# Drop unwanted levels for AVALCAT5 and AVALCAT6 and set levels defined in top section of script

ex$AVAL1_C1 <- droplevels(ex$AVAL1_C1)
ex$AVAL1_C1 <- factor(ex$AVAL1_C1, levels = trtdurlevels)
ex$AVALCAT2 <- droplevels(ex$AVALCAT2)
ex$AVALCAT2 <- factor(ex$AVALCAT2, levels = if (!is.null(dsdyslevels_c1)) dsdyslevels_c1 else levels(ex$AVALCAT2))
ex$AVALCAT6 <- droplevels(ex$AVALCAT6)
ex$AVALCAT6 <- factor(ex$AVALCAT6, levels = mddlevels)
ex$AVALCAT7 <- droplevels(ex$AVALCAT7)
ex$AVALCAT7 <- factor(ex$AVALCAT7, levels = mddlevels)

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

lyt <- lyt

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
    "AVAL1_C1",
    var_labels = "Duration of treatment, n (%)",
    afun = a_freq_j,
    extra_args = extra_args_rr,
    indent_mod = 2L,
    show_labels = "visible",
    nested = TRUE,
    section_div = " "
  ) |>
  analyze(
    paste0("CRIT", 1, "FL"),
    var_labels = "Duration of treatment~[super b], n (%)",
    afun = a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(label = as.character(ex_lblmap$label[1]), val = "Y")
    ),
    indent_mod = 2L,
    show_labels = "visible",
    nested = TRUE
  )

# Add in analyze for all others CRIT variables contained in ex
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
      indent_mod = 3L,
      show_labels = "hidden"
    )
}

lyt <- lyt |>
  analyze(
    "AVAL2",
    nested = FALSE,
    var_labels = "Total dosing days of treatment (excluding days off treatment)~[super c]",
    show_labels = "visible",
    afun = a_summary,
    extra_args = list(
      .stats = aval1_stats,
      .formats = .formats_all0,
      .labels = aval1_labels,
      .indent_mods = aval1_indent_mods
    )
  )
if (show_cat_dsdys && any(!is.na(ex$AVALCAT2))) {
  lyt <- lyt |>
    analyze(
      "AVALCAT2",
      afun = a_freq_j,
      extra_args = extra_args_rr,
      show_labels = "hidden",
      indent_mod = 2L
    )
}
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
  )

if (show_meanddi) {
  lyt <- lyt |>
    analyze(
      "AVAL4",
      nested = FALSE,
      var_labels = sprintf("Average daily dose (%s) (including days off treatment)", get_unit("MEANDDI")),
      show_labels = "visible",
      afun = a_summary,
      extra_args = list(
        .stats = aval1_stats,
        .formats = .formats_all,
        .labels = aval1_labels,
        .indent_mods = aval1_indent_mods
      )
    )
}

if (show_meandd) {
  lyt <- lyt |>
    analyze(
      "AVAL5",
      nested = FALSE,
      var_labels = sprintf("Average daily dose (%s) (excluding days off treatment)", get_unit("MEANDD")),
      show_labels = "visible",
      afun = a_summary,
      extra_args = list(
        .stats = aval1_stats,
        .formats = .formats_all,
        .labels = aval1_labels,
        .indent_mods = aval1_indent_mods
      )
    )
}

if (show_modedd) {
  lyt <- lyt |>
    analyze(
      "AVAL6",
      nested = FALSE,
      var_labels = sprintf("Modal daily dose (%s), n (%%)", get_unit("MODEDD")),
      show_labels = "visible",
      afun = a_summary,
      extra_args = list(
        .stats = c("n"),
        .formats = .formats_all0,
        .labels = aval1_labels,
        .indent_mods = aval1_indent_mods
      )
    )
}

if (show_modedd && show_cat_modedd && any(!is.na(ex$AVALCAT6))) {
  lyt <- lyt |>
    analyze(
      "AVALCAT6",
      afun = a_freq_j,
      extra_args = extra_args_rr,
      show_labels = "hidden",
      indent_mod = 2L
    )
}

if (show_findd) {
  lyt <- lyt |>
    analyze(
      "AVAL7",
      nested = FALSE,
      var_labels = sprintf("Final daily dose (%s)", get_unit("FINDD")),
      show_labels = "visible",
      afun = a_summary,
      extra_args = list(
        .stats = aval1_stats,
        .formats = .formats_all0,
        .labels = aval1_labels,
        .indent_mods = aval1_indent_mods
      )
    )
}

if (show_findd && show_cat_findd && any(!is.na(ex$AVALCAT7))) {
  lyt <- lyt |>
    analyze(
      "AVALCAT7",
      afun = a_freq_j,
      nested = FALSE,
      show_labels = "visible",
      var_labels = sprintf("Final daily dose (%s), n (%%)", get_unit("FINDD")),
      extra_args = extra_args_rr,
      indent_mod = 2L
    )
}

lyt <- lyt |>
  append_topleft("Parameter")

result <- build_table(lyt, ex, alt_counts_df = adsl, round_type = "sas")

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table
################################################################################

colwidth <- c(64, 34, 35, 31)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, label_width_ins = 2.4, orientation = "portrait")
