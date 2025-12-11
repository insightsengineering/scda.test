################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsfae17d.R
## R version:                 4.4.1
## junco Version:             1.0
## Short Description:         Subjects With Treatment-emergent Adverse Events by Organ
##                            System, OCMQ (Broad) and Preferred Term
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      2025-07-21
## Input:                     ADSL, ADAEOCMQ
## Output:                    TSFAE17d.rtf
## Remarks:                   Template R script version using rtables framework
##
## Modification History:
##  Rev #:                    1
##  Modified By:
##  Reporting Effort:         Code Refactoring
##  Date:                     2025-08-13
##  Description:              Refactored cpct_relrisk_fact to a_freq_j
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
# - Define OCMQ Class used (default=Narrow)
# - Choose whether or not you want to present a combined active treatment column (default=TRUE)
# - Choose whether or not you want to present the risk difference columns (default=TRUE)
# - Choose which risk difference method you would like (default=Wald)
# - Define what the control treatment group is for your study (e.g Placebo)
# - Define how to create combined treatment columns (if required)
################################################################################

tblid <- "TSFAE17d"
fileid <- write_path(opath, tblid)
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

trtvar <- "TRT01A"
popfl <- "SAFFL"
ocmqclass <- "Broad"
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
  select(STUDYID, USUBJID, all_of(trtvar), all_of(popfl))

adae_full <- adaeocmq_jnj %>%
  filter(TRTEMFL == "Y" & OCMQCLSS == ocmqclass) %>%
  select(USUBJID, TRTEMFL, OCMQSOC, OCMQNAM, AEDECOD)

adae <- adae_full %>%
  group_by(OCMQSOC) %>%
  slice(1) %>%
  ungroup()

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == "Placebo", " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

if (risk_diff == TRUE) {
  adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
  adsl$rrisk_label <- paste(adsl[[trtvar]], paste("vs", ctrl_grp))
}

# join data together
ae <- adae %>% right_join(., adsl, by = c("USUBJID"))

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

################################################################################
# Define layout and build table:
################################################################################

ref_path <- c("colspan_trt", " ", "TRT01A", "Placebo")
extra_args_rr <- list(
  method = rr_method,
  ref_path = ref_path,
  .stats = c("count_unique_fraction")
)

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

lyt <- lyt %>%
  split_rows_by(
    "OCMQSOC",
    split_label = "Organ System~[super a]",
    split_fun = trim_levels_in_group("OCMQNAM"),
    label_pos = "topleft",
    section_div = c(" "),
    nested = FALSE
  ) %>%
  summarize_row_groups(
    "OCMQSOC",
    cfun = a_freq_j,
    extra_args = append(extra_args_rr, NULL)
  ) %>%
  split_rows_by(
    "OCMQNAM",
    split_label = paste0("OCMQ (", ocmqclass, ")"),
    split_fun = trim_levels_in_group("AEDECOD"),
    label_pos = "topleft",
    nested = TRUE
  ) %>%
  summarize_row_groups(
    "OCMQNAM",
    cfun = a_freq_j,
    extra_args = append(extra_args_rr, NULL)
  ) %>%
  analyze(
    "AEDECOD",
    afun = a_freq_j,
    extra_args = append(extra_args_rr, NULL)
  ) %>%
  append_topleft("    Preferred Term, n (%)")

result <- build_table(lyt, ae, alt_counts_df = adsl)

## Remove the N=xx column headers for the risk difference columns
result <- remove_col_count(result)

# If there is no data display "No data to display" text
if (nrow(adae) == 0) {
  result <- safe_prune_table(result)
}

#########################################################################################
# Post-Processing step to sort by descending count on chosen active treatment columns.
# Default is the last treatment (inc. Combined if applicable) under the active treatment
# spanning header (defaulted to colspan_trt variable). See function documentation for
# jj_complex_scorefun should your require a different sorting behavior.
#########################################################################################

if (nrow(adae) != 0) {
  result <- sort_at_path(
    result,
    c("root", "OCMQSOC"),
    scorefun = jj_complex_scorefun()
  )
  result <- sort_at_path(
    result,
    c("root", "OCMQSOC", "*", "OCMQNAM"),
    scorefun = jj_complex_scorefun()
  )
  result <- sort_at_path(
    result,
    c("root", "OCMQSOC", "*", "OCMQNAM", "*", "AEDECOD"),
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
