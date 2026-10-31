library(envsetup)
library(dplyr)
library(rtables)
library(junco)
library(haven)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSFVIT01"
fileid <- write_path(opath, tblid)
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

# Safety population flag (default=SAFFL).
popfl <- "SAFFL"

# Actual treatment variable (default=TRT01A).
trtvar <- "TRT01A"
# Actual treatment levels (required to order the columns).
trtlev <- c("Xanomeline Low Dose", "Xanomeline High Dose", "Placebo")

# Control arm name (one of the levels of the treatment variable).
ctrl_grp <- "Placebo"

# Flag to enable comparison between treatment groups (e.g., mean difference vs. control).
comp_btw_group <- TRUE

# Add Active Study Agent Combined column?
combined_colspan_trt <- FALSE

# Vital sign parameters of interest.
paramcd <- c("SYSBP", "DIABP")

# Non-baseline visits of interest.
nonbl_visits <- c("Cycle 02", "Cycle 03", "Cycle 04")

################################################################################
# Process data:
################################################################################

adsl <- adsl_jnj |>
  select(USUBJID, all_of(c(popfl, trtvar))) |>
  filter(
    toupper(.data[[popfl]]) == "Y",
    !is.na(.data[[trtvar]])
  ) |>
  mutate(
    !!trtvar := ordered(.data[[trtvar]], levels = trtlev),
  ) |>
  create_colspan_var(
    non_active_grp = ctrl_grp,
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = trtvar
  )

advs <- advs_jnj |>
  select(
    USUBJID,
    all_of(c(popfl, trtvar)),
    AVISITN,
    AVISIT,
    ATPT,
    PARAMCD,
    PARAM,
    PARAMN,
    AVAL,
    BASE,
    CHG,
    starts_with("ANL"),
    ABLFL,
    APOBLFL
  ) |>
  filter(
    !is.na(USUBJID),
    (.data[[popfl]] == "Y"),
    !is.na(.data[[trtvar]]),
    PARAMCD %in% paramcd
  ) |>
  mutate(
    !!trtvar := ordered(.data[[trtvar]], levels = trtlev),
    ABLFL := factor(ifelse(ABLFL == "Y" & !is.na(ABLFL), "Y", "N")),
    APOBLFL := factor(ifelse(APOBLFL == "Y" & !is.na(APOBLFL), "Y", "N")),
    PARAMCD = factor(
      .data$PARAMCD,
      levels = unique(.data[["PARAMCD"]])[order(unique(.data[["PARAMN"]]))]
    ),
    PARAM = factor(
      .data$PARAM,
      levels = unique(.data[["PARAM"]])[order(unique(.data[["PARAMN"]]))]
    ),
    AVISIT = factor(
      ifelse(ABLFL == "Y" & !is.na(ABLFL), "Baseline", as.character(AVISIT)),
      levels = unique(.data[["AVISIT"]])[order(unique(.data[["AVISITN"]]))]
    )
  )

# Reordered nonlbl_visits
nonbl_visits <- levels(advs$AVISIT)[levels(advs$AVISIT) %in% nonbl_visits]

advs <- inner_join(adsl, advs, by = c("USUBJID", popfl, trtvar))

# Safety checks on the data.

advs_checks_unique <- advs |>
  filter(ANL02FL == "Y" & (ABLFL == "Y" | APOBLFL == "Y")) |>
  group_by(USUBJID, PARAMCD, AVISIT) |>
  mutate(n_recsub = n()) |>
  filter(n_recsub > 1)

if (nrow(advs_checks_unique) > 0) {
  message(
    "Your input dataset needs extra attention, as some subjects have more than one record per parameter/visit"
  )
}

advs_checked <- advs |>
  filter(
    toupper(.data[[popfl]]) == "Y",
    toupper(ANL01FL) == "Y",
    toupper(ANL02FL) == "Y"
  ) |>
  mutate(
    flag = if_else(is.na(AVAL), 1L, 0L),
    flag1 = if_else(toupper(APOBLFL) == "Y" & is.na(BASE), 1L, 0L)
  )

if (nrow(filter(advs_checked, flag == 1)) > 0) {
  message("Pay attention: There are tests with missing AVAL: ")
}

if (nrow(filter(advs_checked, flag1 == 1)) > 0) {
  message("Pay attention: There are postbaseline tests without baseline: ")
}

# Keep only records with non-missing AVAL
advs <- filter(advs, !is.na(AVAL))

# varying decimal precision ----
# manual setup example
prec <- dplyr::tibble(PARAMCD = unique(advs$PARAMCD), d = 1)
prec$d[prec$PARAMCD %in% c("DIABP")] <- 2

# alternative: use tidytlg make_precision function
# DTYPE='AVERAGE' records are excluded from decimal precision
# advs_avg <- advs |> filter(!DTYPE=="AVERAGE")

# prec2 <- tidytlg:::make_precision_data(
#   df = advs_avg,
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

# add precision specific format to advs input dataset
advs <- advs |>
  left_join(prec)

################################################################################
# Define layout and build table:
################################################################################

colspan_trt_map <- if (!is.null(ctrl_grp)) {
  create_colspan_map(
    advs,
    non_active_grp = ctrl_grp,
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = trtvar
  )
} else {
  tibble(
    colspan_trt = "Active Study Agent",
    !!trtvar := levels(advs[[trtvar]])
  )
}

split_combined <- if (combined_colspan_trt) {
  # Set up levels and label for the required combined columns.
  add_combo <- add_combo_facet(
    "Combined",
    label = "Combined",
    levels = setdiff(levels(advs[[trtvar]]), ctrl_grp)
  )

  # Choose if any facets need to be removed,
  # e.g remove the combined column for placebo.
  rm_combo_from_placebo <- cond_rm_facets(
    facets = "Combined",
    ancestor_pos = NA,
    value = " ",
    split = "colspan_trt"
  )

  make_split_fun(post = list(add_combo, rm_combo_from_placebo))
} else {
  NULL
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
  append_topleft("Parameter") |>
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) |>
  split_cols_by(trtvar, split_fun = split_combined) |>
  split_rows_by(
    "PARAMCD",
    split_fun = keep_split_levels(paramcd),
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
    split_fun = keep_split_levels(nonbl_visits),
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

result <- build_table(lyt, advs, alt_counts_df = adsl, round_type = "sas")

# Add an empty line after each "Screening" section.
# Use `section_div_at_path()` as a workaround for the missing
# `section_div` argument in `summarize_row_groups()`.
# (Feature request: https://github.com/insightsengineering/rtables/issues/1083)
# The path below is determined from `rtables::row_paths_summary(result)`.
section_div_at_path(result, c("PARAMCD", "*", "@content", last(stats))) <- " "

################################################################################
# Post-Processing:
# - Adjust Combined (if displayed) columns Ns (When sequence of
#   treatments is used, one subject receives more than on treatment. Hence,
#   there are many rows for one unique subjects, while N should represent only
#   unique subjects).
################################################################################

if (combined_colspan_trt) {
  adsl_no_ctrl <- if (is.null(ctrl_grp)) {
    adaper
  } else {
    adsl[adsl[[trtvar]] != ctrl_grp, ]
  }
  n_combined <- length(unique(adsl_no_ctrl$USUBJID))
  colpath_combined <- c("colspan_trt", "Active Study Agent", trtvar, "Combined")
  facet_colcount(result, colpath_combined) <- n_combined
}

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table:
################################################################################


colwidth <- c(64, 41, 41, 41)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "landscape")
