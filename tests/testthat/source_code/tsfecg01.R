library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)
library(haven)
library(stringr)

################################################################################
# Define script level parameters:
################################################################################

################################################################################
# - Define output ID and file location
# - Define treatment variable used (default=TRT01A)
# - Define population flag used (default=SAFFL)
# - Define control group label
# - Define ECG parameters and non-baseline visits to include
# - Choose whether or not you want to present a combined active treatment column (default=TRUE)
################################################################################

tblid <- "TSFECG01"
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

# Flag to enable comparison between treatment groups (e.g., mean difference vs. placebo).
comp_btw_group <- TRUE

# ECG parameters of interest (PARAMCD values)

selparamcd <- c("EGHRMN", "QTC", "QTCBAG", "QTCFAG", "RRAG", "QRSAG", "PRAG")

# Non-baseline visits of interest (AVISIT values, excluding Baseline)
nonbl_visits <- c(
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

### available ECG parameters in study
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
    AVAL,
    BASE,
    CHG,
    starts_with("ANL"),
    ABLFL,
    APOBLFL,
    DTYPE
  ) |>
  filter(
    !is.na(USUBJID),
    .data[[popfl]] == "Y",
    !is.na(.data[[trtvar]]),
    PARAMCD %in% selparamcd
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

# restrict to these - ordered by PARAMN
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
    AVISIT := factor(
      ifelse(ABLFL == "Y" & !is.na(ABLFL), "Baseline", as.character(AVISIT)),
      levels = unique(.data[["AVISIT"]])[order(unique(.data[["AVISITN"]]))]
    )
  )

nonbl_visits <- unique(as.character(adeg$AVISIT[adeg$AVISIT %in% nonbl_visits]))

adeg <- inner_join(adsl, adeg, by = c("STUDYID", "USUBJID", popfl, trtvar))

# Safety check: unique record per subject/parameter/visit
adeg_check_unique <- adeg |>
  filter(ANL02FL == "Y" & (ABLFL == "Y" | APOBLFL == "Y")) |>
  group_by(USUBJID, PARAMCD, AVISIT) |>
  mutate(n_recsub = n()) |>
  filter(n_recsub > 1)

if (nrow(adeg_check_unique) > 0) {
  stop(
    "Your input dataset needs extra attention, as some subjects have more than one record per parameter/visit"
  )
}


adeg_checked <- adeg |>
  filter(
    toupper(.data[[popfl]]) == "Y",
    toupper(ANL01FL) == "Y",
    toupper(ANL02FL) == "Y"
  ) |>
  mutate(
    flag = if_else(is.na(AVAL), 1L, 0L),
    flag1 = if_else(toupper(APOBLFL) == "Y" & is.na(BASE), 1L, 0L)
  )

if (nrow(filter(adeg_checked, flag == 1)) > 0) {
  stop("Pay attention: There are tests with missing AVAL: ")
}

if (nrow(filter(adeg_checked, flag1 == 1)) > 0) {
  stop("Pay attention: There are postbaseline tests without baseline: ")
}

# Keep only records with non-missing AVAL
adeg <- filter(adeg, !is.na(AVAL))


################################################################################
# Formats for decimal precision control
################################################################################

# manual setup example for user
prec <- dplyr::tibble(PARAMCD = selparamcd, d = 1)
prec$d[prec$PARAMCD == "EGHRMN"] <- 0

# alternative: use tidytlg make_precision function
# cutoff for decimal is defined in function and if DTYPE="AVERAGE" those records excluded from precison

# adeg_avg <- adeg |> filter(!DTYPE=="AVERAGE")

# prec2 <- tidytlg:::make_precision_data(
#   df = adeg_avg,
#   decimal = 4,
#   precisionby = "PARAMCD",
#   precisionon = "AVAL"
# ) |>
#   rename(c(d = "decimal"))

.stats_all <- c("n", "mean_sd", "median_range", "diff_means_est_ci")
# add d-based formats for all required stats onto precision dataset
prec <- fmt_spec_df_d(
  prec, # If using alternative method of precison add that dataset here
  d_column = "d",
  fmt_column = "fmt_d",
  stats_in = .stats_all,
  fmt_d_def = junco_def_d_all,
  fmt_d_in = NULL
)

# If user wanted to review d-based formats below code should be used

# fmt_d_details <- lapply(prec$PARAMCD,
#                         FUN = function(x) {
#                           fmt <- prec[prec$PARAMCD == x,][["fmt_d"]][[1]]
#                           get_fmt_details(
#                             fmt,
#                             as_tibble = TRUE)
#                         }
# )
# names(fmt_d_details) <- prec$PARAMCD
# fmt_d_details

# add precision specific format to adeg input dataset
adeg <- adeg |>
  left_join(prec)

################################################################################
# Define layout and build table:
################################################################################

colspan_trt_map <- if (!is.null(ctrl_grp)) {
  create_colspan_map(
    adeg,
    non_active_grp = ctrl_grp,
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = trtvar
  )
} else {
  tibble(
    colspan_trt = "Active Study Agent",
    !!trtvar := levels(adeg[[trtvar]])
  )
}


# Common stats for Baseline / Timepoint / Change sections
stats <- c("n", "mean_sd", "median_range")
labels <- junco_get_labels_from_stats(stats, labels_in = c(n = "N"))
indent_mods <- c(n = 1L, mean_sd = 2L, median_range = 2L)

# Dynamic "Change from baseline to <visit>" section label
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
  # .formats = c(diff_means_est_ci = jjcsformat_xx("xx.xx (xx.xx, xx.xx)")),
  .labels = c(
    diff_means_est_ci = paste0(
      "Difference in mean [vs. ",
      str_to_lower(ctrl_grp),
      "]",
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
  )

if (combined_colspan_trt == TRUE) {
  lyt <- lyt |>
    split_cols_by(trtvar, split_fun = mysplit)
} else {
  lyt <- lyt |>
    split_cols_by(trtvar)
}

# Row split by PARAM (PARAM/PARAMN label per mock annotation)
lyt <- lyt |>
  split_rows_by(
    "PARAMCD",
    split_fun = keep_split_levels(selparamcd),
    labels_var = "PARAM",
    child_labels = "visible"
  ) |>
  # ── Baseline section (ABLFL = "Y") ──────────────────────────────────────────
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
  # ── Non-baseline timepoints (APOBLFL = "Y" & ANL02FL = "Y") ─────────────────
  split_rows_by(
    "AVISIT",
    split_fun = keep_split_levels(nonbl_visits),
    indent_mod = -1,
    section_div = " "
  ) |>
  # Timepoint: Mean (SD) and Median (min, max) of AVAL
  analyze(
    "AVAL",
    afun = tern::a_summary,
    extra_args = list(
      .stats = stats,
      .formats = "default",
      .labels = labels,
      .indent_mods = indent_mods - 1L,
      na_rm = TRUE
    ),
    formats_var = "fmt_d",
    show_labels = "hidden",
    section_div = " "
  ) |>
  # Change from baseline section label (dynamic: "Change from baseline to <visit>")
  analyze(
    "CHG",
    afun = a_chg_label,
    table_names = "chg_bl_label",
    show_labels = "hidden",
    indent_mod = -1L
  ) |>
  # N for change (subjects with non-missing CHG)
  analyze(
    "CHG",
    afun = tern::a_summary,
    extra_args = list(
      .stats = "n",
      .formats = "default",
      .labels = labels,
      na_rm = TRUE
    ),
    formats_var = "fmt_d",
    show_labels = "hidden"
  ) |>
  # Baseline mean (SD) — among subjects with non-missing CHG
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
    table_names = "chg_bl_mean",
    show_labels = "hidden",
    indent_mod = 1L,
    formats_var = "fmt_d"
  ) |>
  # Mean (SD) and Median (min, max) of CHG
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
    # Difference in mean [vs placebo] (95% CI) — active arms only
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

result <- build_table(lyt, adeg, alt_counts_df = adsl, round_type = "sas")


# Add empty line after each Baseline section (workaround for missing section_div
# in summarize_row_groups; see https://github.com/insightsengineering/rtables/issues/1083)
section_div_at_path(result, c("PARAMCD", "*", "@content", last(stats))) <- " "


################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table
################################################################################


colwidth <- c(64, 40, 43, 38, 39)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "landscape")
