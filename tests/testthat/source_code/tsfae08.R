################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsfae08.R
## R version:                 4.2.1
## junco Version:             1.0
## Short Description:         Program to create tsfae08: AE table by SOC/PT (excl. serious AEs) - TEAEs > x%
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      12 Dec 2023
## Input:                     ADSL, ADAE.
## Output:                    TSFAE08.rtf
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
# - Choose whether or not you want to present a combined active treatment column (default=TRUE)
# - Define how to create combined treatment columns (if required)
################################################################################

tblid <- "TSFAE08"
fileid <- write_path(opath, tblid)
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

trtvar <- "TRT01A"
popfl <- "SAFFL"
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

adsl <- adsl_jnj %>%
  filter(!!rlang::sym(popfl) == "Y") %>%
  select(STUDYID, USUBJID, all_of(trtvar), all_of(popfl))

adae <- adae_jnj %>%
  filter(TRTEMFL == "Y" & AESER == "N") %>%
  select(USUBJID, TRTEMFL, AEBODSYS, AEDECOD)

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == "Placebo", " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

# join data together
ae <- adae %>% right_join(., adsl, by = c("USUBJID"))

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = "Placebo",
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

################################################################################
# Define layout and build table:
################################################################################

extra_args_1 <- list(
  denom = "n_altdf",
  .stats = c("count_unique_fraction")
)


lyt <- rtables::basic_table(
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

lyt <- lyt %>%
  analyze(
    "TRTEMFL",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = append(
      extra_args_1,
      list(label = "Subjects with >=1 AE", val = "Y")
    )
  ) %>%
  split_rows_by(
    "AEBODSYS",
    child_labels = "hidden",
    split_label = "System Organ Class",
    label_pos = "topleft",
    split_fun = trim_levels_in_group("AEDECOD"),
    section_div = c(" "),
    indent_mod = 0L
  ) %>%
  summarize_row_groups(
    "AEBODSYS",
    cfun = a_freq_j,
    extra_args = extra_args_1
  ) %>%
  analyze("AEDECOD", afun = a_freq_j, extra_args = extra_args_1) %>%
  append_topleft("  Preferred Term, n (%)")

result <- build_table(lyt, ae, alt_counts_df = adsl)

# If there is no data remove top row and display "No data to display" text
if (length(adae$TRTEMFL) == 0) {
  result <- safe_prune_table(
    result,
    prune_func = remove_rows(removerowtext = "Subjects with >=1 AE")
  )
}

#########################################################################################
# Post-Processing step to sort by descending count on chosen active treatment columns.
# Default is the last treatment (inc. Combined if applicable) under the active treatment
# spanning header (defaulted to colspan_trt variable). See function documentation for
# jj_complex_scorefun should your require a different sorting behavior.
#########################################################################################

if (length(adae$TRTEMFL) != 0) {
  result <- sort_at_path(
    result,
    c("root", "AEBODSYS"),
    scorefun = jj_complex_scorefun()
  )
  result <- sort_at_path(
    result,
    c("root", "AEBODSYS", "*", "AEDECOD"),
    scorefun = jj_complex_scorefun()
  )

  ################################################################################
  # Prune table to only keep those that meet x% criteria, but top row is not subject to pruning
  ################################################################################
  result <- safe_prune_table(
    result,
    prune_func = bspt_pruner(
      fraction = 0.05,
      keeprowtext = "Subjects with >=1 AE",
      cols = c(trtvar)
    )
  )
}

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table
################################################################################

tt_to_tlgrtf(result, file = fileid, orientation = "portrait")
