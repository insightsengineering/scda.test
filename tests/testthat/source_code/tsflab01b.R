library(envsetup)
library(dplyr)
library(rtables)
library(junco)
library(haven)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSFLAB01b"
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

# Population flag (default=SAFFL).
popfl <- "SAFFL"

# Actual treatment variable (default=TRT01A).
trtvar <- "TRT01A"

# Control arm name (one of the levels of the treatment variable).
ctrl_grp <- "Placebo"

# Lab parameters Category of interest on PARCAT1 (name = file suffix).
param_cat <- c(chm = "CHEMISTRY") # , hem = "HEMATOLOGY")

# Lab sign parameters of interest.
param_cd <- NULL

# Non-baseline visits of interest.
nonbl_visits <- c("Cycle 02", "Cycle 04", "Cycle 03")

# Flag to enable comparison between treatment groups (e.g., mean difference vs. control).
comp_btw_group <- TRUE

# Add Active Study Agent Combined column
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
# Process data:
################################################################################

adsl <- pharmaverseadamjnj::adsl |>
  select(USUBJID, all_of(c(popfl, trtvar))) |>
  filter(
    toupper(.data[[popfl]]) == "Y",
    !is.na(.data[[trtvar]])
  ) |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c("Xanomeline Low Dose", "Xanomeline High Dose", "Placebo")
    )
  ) |>
  create_colspan_var(
    non_active_grp = ctrl_grp,
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = trtvar
  )

adlb <- pharmaverseadamjnj::adlb |>
  select(
    USUBJID,
    all_of(c(popfl, trtvar)),
    AVISITN,
    AVISIT,
    ATPT,
    PARAMCD,
    PARAM,
    PARAMN,
    PARCAT1,
    PARCAT3,
    PARCAT3N,
    AVAL,
    BASE,
    CHG,
    starts_with("ANL"),
    ABLFL,
    APOBLFL
  ) |>
  filter(
    (.data[[popfl]] == "Y"),
    !is.na(.data[[trtvar]]),
    if (!is.null(param_cat)) PARCAT1 %in% param_cat else TRUE,
    if (!is.null(param_cd)) PARAMCD %in% param_cd else TRUE
  ) |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c("Xanomeline Low Dose", "Xanomeline High Dose", "Placebo")
    ),
    ABLFL := factor(ifelse(ABLFL == "Y" & !is.na(ABLFL), "Y", "N")),
    APOBLFL := factor(ifelse(APOBLFL == "Y" & !is.na(APOBLFL), "Y", "N")),
    PARAMCD := ordered(PARAMCD, levels = {
      # Sort by PARCAT3N then PARAMCD and PARCAT3 not displayed used only for sorting
      unique(PARAMCD[order(PARCAT3N, PARAM)])
    }),
    PARAM := ordered(PARAM, levels = {
      # Sort by PARCAT3N then PARAM and PARCAT3 not displayed used only for sorting
      unique(PARAM[order(PARCAT3N, PARAM)])
    }),
    AVISIT = factor(
      ifelse(ABLFL == "Y" & !is.na(ABLFL), "Baseline", as.character(AVISIT)),
      levels = unique(.data[["AVISIT"]])[order(unique(.data[["AVISITN"]]))]
    )
  )

# Reordered nonlbl_visits
nonbl_visits <- unique(as.character(adlb$AVISIT[adlb$AVISIT %in% nonbl_visits]))

adlb <- inner_join(adsl, adlb, by = c("USUBJID", popfl, trtvar))

# Safety checks on the data.
adlb_checks_unique <- adlb |>
  filter(ANL02FL == "Y" & (ABLFL == "Y" | APOBLFL == "Y")) |>
  group_by(USUBJID, PARCAT1, PARAMCD, AVISIT) |>
  mutate(n_recsub = n()) |>
  filter(n_recsub > 1)

if (nrow(adlb_checks_unique) > 0) {
  stop(
    "Your input dataset needs extra attention, as some subjects have more than one record per parameter/visit"
  )
}

adlb_checked <- adlb |>
  filter(
    toupper(.data[[popfl]]) == "Y",
    toupper(ANL01FL) == "Y",
    toupper(ANL02FL) == "Y"
  ) |>
  mutate(
    flag = if_else(is.na(AVAL), 1L, 0L),
    flag1 = if_else(toupper(APOBLFL) == "Y" & is.na(BASE), 1L, 0L)
  )

if (nrow(filter(adlb_checked, flag == 1)) > 0) {
  message("Pay attention: There are tests with missing AVAL: ")
}

if (nrow(filter(adlb_checked, flag1 == 1)) > 0) {
  message("Pay attention: There are postbaseline tests without baseline: those will be removed")
}

# Keep only records with non-missing AVAL
adlb <- filter(adlb, !is.na(AVAL))

# varying decimal precision ----
# manual setup example
prec <- dplyr::tibble(PARAMCD = unique(adlb$PARAMCD), d = 1)
prec$d[prec$PARAMCD %in% c("ALB", "CA")] <- 2

# alternative: use tidytlg make_precision function
# DTYPE='AVERAGE' records are excluded from decimal precision
# adlb_avg <- adlb |> filter(!DTYPE=="AVERAGE")

# prec2 <- tidytlg:::make_precision_data(
#   df = adlb_avg,
#   decimal = 4,
#   precisionby = "PARAMCD",
#   precisionon = "AVAL"
# ) |>
#   rename(c(d = "decimal"))

.stats_all <- c("n", "mean_sd", "median_range", "diff_means_est_ci")
# add d-based formats for all required stats onto precision dataset
prec <- fmt_spec_df_d(
  prec,
  d_column = "d",
  fmt_column = "fmt_d",
  stats_in = .stats_all,
  fmt_d_def = junco_def_d_all,
  fmt_d_in = NULL
)

# review d-based formats
fmt_d_details <- lapply(prec$PARAMCD, FUN = function(x) {
  fmt <- prec[prec$PARAMCD == x, ][["fmt_d"]][[1]]
  get_fmt_details(
    fmt,
    as_tibble = TRUE
  )
})
names(fmt_d_details) <- prec$PARAMCD
# fmt_d_details

# add precision specific format to adlb input dataset
adlb <- adlb |>
  left_join(prec)

################################################################################
# Define layout and build table (loop over categories):
################################################################################

cat_suffix <- param_cat
adlb_cat <- filter(adlb, PARCAT1 == param_cat[[1]]) |>
  filter(ABLFL == "Y" | ANL02FL == "Y" | APOBLFL == "Y")
fileid <- write_path(opath, paste0(tblid))

colspan_trt_map <- if (!is.null(ctrl_grp)) {
  create_colspan_map(
    adlb_cat,
    non_active_grp = ctrl_grp,
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = trtvar
  )
} else {
  tibble(
    colspan_trt = "Active Study Agent",
    !!trtvar := levels(adlb[[trtvar]])
  )
}

# Common stats for time point / change from baseline to time point.
stats <- c("n", "mean_sd", "median_range")
formats <- junco_get_formats_from_stats(stats)
labels <- junco_get_labels_from_stats(stats, labels_in = c(n = "N"))
indent_mods <- c(n = 1L, mean_sd = 2L, median_range = 2L)

# Dynamic label for the "Change from baseline ..." sections.
a_chg_label <- function(x, .spl_context) {
  last_split <- length(.spl_context$split)
  label <- paste("Change from baseline to", .spl_context$value[last_split])
  rtables::rcell(NULL, label = label)
}

# Difference in means parameters.
CI_cl <- tern::control_analyze_vars()$conf_level
a_diff_means_args <- list(
  ref_path = c("colspan_trt", " ", trtvar, ctrl_grp),
  conf.level = CI_cl,
  .stats = "diff_means_est_ci",
  .formats = "default",
  .labels = c(
    diff_means_est_ci = paste0(
      "Difference in mean vs. ",
      tolower(ctrl_grp),
      " (",
      tern::f_conf_level(CI_cl),
      ")"
    )
  )
)

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx",
  top_level_section_div = " "
) |>
  append_topleft("Laboratory Test") |>
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
  split_rows_by(
    "PARAMCD",
    split_fun = if (!is.null(param_cd)) keep_split_levels(param_cd) else drop_split_levels,
    labels_var = "PARAM",
    child_labels = "visible"
  ) |>
  summarize_row_groups(
    "AVAL",
    cfun = c_summary_subset_label,
    extra_args = list(
      filter_expr = expression(ABLFL == "Y"),
      .stats = stats,
      .formats = "default",
      formats_var = "fmt_d",
      .labels = labels,
      .indent_mods = indent_mods,
      label = "Baseline"
    )
  ) |>
  split_rows_by(
    "AVISIT",
    split_fun = if (!is.null(nonbl_visits)) keep_split_levels(nonbl_visits) else drop_split_levels,
    indent_mod = -1,
    section_div = " ",
  ) |>
  analyze(
    "AVAL",
    afun = tern::a_summary,
    extra_args = list(
      .stats = stats,
      .formats = "default",
      .labels = labels,
      .indent_mods = indent_mods - 1,
      na_rm = TRUE
    ),
    formats_var = "fmt_d",
    show_labels = "hidden",
    section_div = " "
  ) |>
  analyze(
    "CHG",
    afun = a_chg_label,
    table_names = "chg_bl_label",
    show_labels = "hidden",
    indent_mod = -1L
  ) |>
  analyze(
    "CHG",
    afun = tern::a_summary,
    extra_args = list(
      .stats = "n",
      .formats = "default",
      .labels = labels,
      na_rm = TRUE # Subjects with non-missing values at both baseline and the postbaseline time point.
    ),
    formats_var = "fmt_d",
    show_labels = "hidden"
  ) |>
  analyze(
    "BASE",
    afun = a_summary_subset,
    extra_args = list(
      filter_expr = expression(!is.na(CHG)),
      na_rm = TRUE,
      .stats = "mean_sd",
      .formats = "default",
      .labels = c(mean_sd = "Baseline mean (SD)")
    ),
    formats_var = "fmt_d",
    table_names = "chg_bl_mean",
    show_labels = "hidden",
    indent_mod = 1L
  ) |>
  analyze(
    "CHG",
    afun = tern::a_summary,
    extra_args = list(
      .stats = c("mean_sd", "median_range"),
      .formats = "default",
      .labels = labels,
      na_rm = TRUE
    ),
    formats_var = "fmt_d",
    table_names = "chg_mm",
    show_labels = "hidden",
    indent_mod = 1L
  )

if (comp_btw_group) {
  lyt <- lyt |>
    analyze(
      "CHG",
      afun = a_diff_means,
      extra_args = a_diff_means_args,
      formats_var = "fmt_d",
      table_names = "chg_diff_means",
      show_labels = "hidden",
      indent_mod = 1L
    )
}

result <- build_table(lyt, adlb_cat, alt_counts_df = adsl, round_type = "sas")

# Add an empty line after each "Screening" section.
# Use `section_div_at_path()` as a workaround for the missing
# `section_div` argument in `summarize_row_groups()`.
# (Feature request: https://github.com/insightsengineering/rtables/issues/1083)
# The path below is determined from `rtables::row_paths_summary(result)`.
section_div_at_path(result, c("PARAMCD", "*", "@content", last(stats))) <- " "

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table:
################################################################################


colwidth <- c(64, 37, 37, 37, 34)

x <- tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "landscape")
