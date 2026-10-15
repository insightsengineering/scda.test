library(envsetup)
library(dplyr)
library(forcats)
library(rtables)
library(tern)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TPK02"
fileid <- write_path(opath, tblid)
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)
popfl <- "PKFL"
trtvar <- "TRT01A"
paramcd <- "XAN"

# Subset variable: "WGTGR1" (body weight quartiles) or "BMIGR1" (BMI quartiles)
# or study-specific category variable (refer to SAP)
subgrpvar <- "WGTGR1"
subgrp_label <- "Body Weight (kg)"

# Flags indicating whether to include certain statistics in the table
add_geometric_mean <- TRUE
add_cv <- TRUE
add_interquartile_range <- TRUE


# flag to indicate if ATPT is present in the study
# if TRUE, time point is concatenation of AVISIT and ATPT
# if FALSE, time point is AVISIT only
use_atpt <- TRUE

trt_grps <- c("Xanomeline Low Dose", "Xanomeline High Dose")

################################################################################
# Process data:
################################################################################

adsl <- adsl_jnj |>
  filter(.data[[popfl]] == "Y") |>
  filter(.data[[trtvar]] == trt_grps) |>
  select(STUDYID, USUBJID, all_of(c(trtvar, popfl, subgrpvar))) |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = trt_grps
    ),
    colspan = subgrp_label
  )

adpc <- adpc_jnj |>
  filter(PARAMCD == paramcd) |> # If not required this filter can be removed
  select(STUDYID, USUBJID, AVISIT, AVISITN, any_of(c("ATPT", "ATPTN")), AVAL) |>
  inner_join(adsl, by = c("STUDYID", "USUBJID"))

# derive selvisit from data, ordered by AVISITN then ATPTN
selvisit <- if (use_atpt) {
  adpc |>
    arrange(AVISITN, ATPTN) |>
    distinct(AVISIT, ATPT) |>
    mutate(AVISIT_ATPT = paste(AVISIT, ATPT, sep = ", ")) |>
    pull(AVISIT_ATPT)
} else {
  adpc |>
    arrange(AVISITN) |>
    distinct(AVISIT) |>
    pull(AVISIT)
}

adpc <- adpc |>
  mutate(
    AVISIT_ATPT := factor(
      if (use_atpt) paste(AVISIT, ATPT, sep = ", ") else as.character(AVISIT),
      levels = selvisit
    )
  )

################################################################################
# Define layout and build table:
################################################################################

lyt <- basic_table() |>
  split_cols_by(
    var = trtvar,
    show_colcounts = TRUE,
    colcount_format = "N=xx",
    split_fun = drop_split_levels
  ) |>
  split_cols_by(
    var = "colspan",
    split_fun = drop_split_levels
  ) |>
  split_cols_by(
    var = subgrpvar,
    split_fun = drop_split_levels
  ) |>
  split_rows_by(
    var = "AVISIT_ATPT",
    split_label = "Time Point",
    label_pos = "topleft",
    section_div = " ",
    split_fun = drop_split_levels
  ) |>
  analyze(
    vars = "AVAL",
    afun = a_summary,
    extra_args = list(
      .stats = c(
        "n",
        "mean_sd",
        "median",
        if (add_geometric_mean) "geom_mean" else NULL,
        "range",
        if (add_cv) "cv" else NULL,
        if (add_interquartile_range) "quantiles" else NULL
      ),
      .labels = c(
        n = "N",
        mean_sd = "Mean (SD)",
        median = "Median",
        geom_mean = "Geometric mean",
        range = "Min, max",
        cv = "CV (%)",
        quantiles = "Interquartile range"
      ),
      .formats = c(
        n = jjcsformat_xx("xx"),
        mean_sd = format_sigfig_j(3, format = "xx (xx)"),
        median = format_sigfig_j(3, format = "xx"),
        geom_mean = format_sigfig_j(3, format = "xx"),
        range = format_sigfig_j(3, format = "(xx, xx)"),
        cv = jjcsformat_xx("xx.x"),
        quantiles = format_sigfig_j(3, format = "(xx, xx)")
      ),
      control = control_analyze_vars(
        quantiles = c(0.25, 0.75),
        quantile_type = 2
      ),
      .indent_mods = c(
        n = 0L,
        mean_sd = 1L,
        median = 1L,
        geom_mean = 1L,
        range = 1L,
        cv = 1L,
        quantiles = 1L
      )
    )
  )

result <- build_table(lyt, df = adpc, alt_counts_df = adsl, round_type = "sas")

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table:
################################################################################


colwidth <- c(41, 31, 29, 31, 29, 31, 31, 29, 29)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "portrait", combined_rtf = TRUE, nosplitin = list(cols = trtvar))
