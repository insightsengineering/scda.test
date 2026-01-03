################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsfdth01.R
## R version:                 4.2.1
## junco version:             1.0
## Short Description:         Program to create tsfdth01: Table of deaths
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      15 Jan 2024
## Input:                     ADSL
## Output:                    TSFDTH01.rtf
## Remarks:                   Template R script version using rtables framework
##
## Modification History:
##  Rev #:                    1
##  Modified By:
##  Reporting Effort:
##  Date:
##  Description:
################################################################################

################################################################################
# Prep Environment
################################################################################

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
# - Define the number of days required for death within xx days (ie how DTHTRTFL and DTHAFTFL were defined)
# - Define the cause of death variable to be used. DTHCAUS is defaulted as the primary cause of death.
# - Choose whether or not you want to present a combined active treatment column (default=TRUE)
# - Choose whether or not you want to present the risk difference columns (default=TRUE)
# - Choose which risk difference method you would like (default=Wald)
# - Define what the control treatment group is for your study (e.g Placebo)
# - Define how to create combined treatment columns (if required)
################################################################################

tblid <- "TSFDTH01"
fileid <- write_path(opath, tblid)
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

trtvar <- "TRT01A"
popfl <- "SAFFL"

days <- 30
dthcausevar <- "DTHCAUS"

combined_colspan_trt <- TRUE
risk_diff <- TRUE
rr_method <- "wald"
ctrl_grp <- "Placebo"

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

adsl <- adsl_jnj %>%
  filter(!!rlang::sym(popfl) == "Y") %>%
  # remove DTHB60FL variable from below if Deaths within 60 days section is not required
  select(
    STUDYID,
    USUBJID,
    all_of(trtvar),
    all_of(popfl),
    DTHFL,
    DTHTRTFL,
    DTHB60FL,
    DTHAFTFL,
    all_of(dthcausevar)
  )

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == "Placebo", " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

if (risk_diff == TRUE) {
  adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
  adsl$rrisk_label <- paste(adsl[[trtvar]], paste("vs", ctrl_grp))
}

adsl <- adsl %>%
  mutate(
    DTHVAR = !!as.name(dthcausevar),
    DTHVAR = as.factor(DTHVAR)
  )

# re-order so OTHER becomes the last category in the table (if there is an OTHER)
is_other <- adsl %>%
  filter(DTHVAR == "OTHER")

if (length(is_other$DTHVAR) != 0) {
  adsl <- adsl %>%
    mutate(DTHVAR = forcats::fct_relevel(DTHVAR, "OTHER", after = Inf))
}

# Convert Reasons to sentence case
adsl$DTHVAR <- stringr::str_to_sentence(adsl$DTHVAR)
adsl$DTHVAR <- as.factor(adsl$DTHVAR)

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)
ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)

################################################################################
# Define layout and build table:
################################################################################

extra_args_rr <- list(
  method = rr_method,
  .stats = c("count_unique_fraction"),
  ref_path = ref_path,
  riskdiff = TRUE,
  denom = "n_altdf"
)

# check if we actually have any deaths so this can be used for the layout
anydth <- adsl %>%
  filter(DTHFL == "Y")

lyt <- basic_table(
  top_level_section_div = " ",
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) %>%
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  )

if (combined_colspan_trt == TRUE) {
  lyt <- lyt %>%
    split_cols_by(trtvar, split_fun = mysplit)
} else {
  lyt <- lyt %>%
    split_cols_by(trtvar)
}

if (risk_diff == TRUE) {
  lyt <- lyt %>%
    split_cols_by("rrisk_header", nested = FALSE) %>%
    split_cols_by(
      trtvar,
      labels_var = "rrisk_label",
      split_fun = remove_split_levels("Placebo")
    )
}

if (length(anydth$DTHFL) != 0) {
  lyt <- lyt %>%
    split_rows_by(
      "DTHFL",
      split_fun = keep_split_levels("Y"),
      split_label = "Deaths",
      label_pos = "topleft",
      section_div = " "
    ) %>%
    summarize_row_groups(
      "DTHFL",
      cfun = a_freq_j,
      extra_args = append(extra_args_rr, list(label = "Total deaths"))
    ) %>%
    analyze(
      "DTHVAR",
      var_labels = " ",
      a_freq_j,
      extra_args = extra_args_rr,
      indent_mod = 0,
      show_labels = "hidden"
    ) %>%
    split_rows_by(
      "DTHTRTFL",
      split_fun = keep_split_levels("Y"),
      section_div = " "
    ) %>%
    summarize_row_groups(
      "DTHTRTFL",
      cfun = a_freq_j,
      extra_args = append(
        extra_args_rr,
        list(label = paste0("Deaths within ", days, " days of last dose"))
      )
    ) %>%
    analyze(
      "DTHVAR",
      var_labels = " ",
      afun = a_freq_j,
      extra_args = extra_args_rr,
      indent_mod = 0,
      show_labels = "hidden"
    ) %>%
    split_rows_by(
      "DTHAFTFL",
      split_fun = keep_split_levels("Y"),
      section_div = " "
    ) %>%
    summarize_row_groups(
      "DTHAFTFL",
      cfun = a_freq_j,
      extra_args = append(
        extra_args_rr,
        list(label = paste0("Deaths beyond ", days, " days of last dose"))
      )
    ) %>%
    analyze(
      "DTHVAR",
      var_labels = " ",
      afun = a_freq_j,
      extra_args = extra_args_rr,
      indent_mod = 0,
      show_labels = "hidden"
    ) %>%
    # Remove below section if the Deaths within 60 days section is not required
    split_rows_by(
      "DTHB60FL",
      split_fun = keep_split_levels("Y"),
      section_div = " "
    ) %>%
    summarize_row_groups(
      "DTHB60FL",
      cfun = a_freq_j,
      extra_args = append(
        extra_args_rr,
        list(label = "Deaths within 60 days of first dose")
      )
    ) %>%
    analyze(
      "DTHVAR",
      var_labels = " ",
      afun = a_freq_j,
      extra_args = extra_args_rr,
      indent_mod = 0,
      show_labels = "hidden"
    )
} else {
  lyt <- lyt %>%
    analyze(
      "DTHFL",
      a_freq_j,
      show_labels = "hidden",
      extra_args = append(extra_args_rr, list(label = "Total deaths"))
    )
}

lyt <- lyt %>%
  append_topleft("  Cause of Death, n (%)")

result <- build_table(lyt, adsl, alt_counts_df = adsl)

## Remove the N=xx column headers for the risk difference columns
result <- remove_col_count(result)

# If there is no deaths remove top row and display "No data to display" text
if (length(anydth$DTHFL) == 0) {
  result <- safe_prune_table(
    result,
    prune_func = remove_rows(removerowtext = "Total deaths")
  )
}

#########################################################################################
# Post-Processing step to sort by descending count on chosen active treatment columns.
# Default is the last treatment (inc. Combined if applicable) under the active treatment
# spanning header (defaulted to colspan_trt variable). See function documentation for
# jj_complex_scorefun should your require a different sorting behavior.
#########################################################################################

if (length(anydth$DTHFL) != 0) {
  result <- sort_at_path(
    result,
    c("root", "DTHFL", "*", "DTHVAR"),
    scorefun = jj_complex_scorefun()
  )
  result <- sort_at_path(
    result,
    c("root", "DTHTRTFL", "*", "DTHVAR"),
    scorefun = jj_complex_scorefun()
  )
  result <- sort_at_path(
    result,
    c("root", "DTHAFTFL", "*", "DTHVAR"),
    scorefun = jj_complex_scorefun()
  )
  result <- sort_at_path(
    result,
    c("root", "DTHB60FL", "*", "DTHVAR"),
    scorefun = jj_complex_scorefun()
  )
}

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table
################################################################################

tt_to_tlgrtf(result, file = fileid, orientation = "landscape")
