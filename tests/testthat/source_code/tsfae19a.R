################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsfae19a.R
## R version:                 4.4.1
## junco Version:             1.0
## Short Description:         Subjects With Treatment-emergent Adverse Events by
##                            Male-specific OCMQ (Narrow) and Preferred Term
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      2025-07-21
## Input:                     ADSL, ADAEOCMQ
## Output:                    TSFAE19a.rtf
## Remarks:                   Template R script version using rtables framework
##
## Modification History:
##  Rev #:                    1
##  Modified By:
##  Reporting Effort:         Code Refactoring
##  Date:                     2025-08-13
##  Description:              Refactored two_tier_cpct_relrisk to a_freq_j
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
# - Define sex to filter on (default=M)
# - Define the OCMQ class to filter on (default=Narrow)
# - Define the OCMQ gender flag to filter on (default=GENSPMFL)
# - Define the list of OCMQNAMs that must appear in the table
# - Choose whether or not you want to present a combined active treatment column (default=TRUE)
# - Choose whether or not you want to present the risk difference columns (default=TRUE)
# - Choose which risk difference method you would like (default=Wald)
# - Define what the control treatment group is for your study (e.g Placebo)
# - Define how to create combined treatment columns (if required)
################################################################################

tblid <- "TSFAE19a"
fileid <- write_path(opath, tblid)
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

trtvar <- "TRT01A"
popfl <- "SAFFL"
sex <- "M"
ocmqclass <- "Narrow"
ocmqflag <- "GENSPMFL"
ocmqnam_list <- c("Erectile Dysfunction", "Gynecomastia")
combined_colspan_trt <- FALSE
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
  filter(!!rlang::sym(popfl) == "Y" & SEX == sex) %>%
  select(STUDYID, USUBJID, all_of(trtvar), all_of(popfl))

adae <- adaeocmq_jnj %>%
  filter(
    TRTEMFL == "Y" & OCMQCLSS == ocmqclass & (!!rlang::sym(ocmqflag) == "Y")
  ) %>%
  select(USUBJID, TRTEMFL, OCMQNAM, AEDECOD, !!rlang::sym(ocmqflag)) %>%
  mutate(
    OCMQNAM = factor(OCMQNAM, levels = union(levels(OCMQNAM), ocmqnam_list)),
  )

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == "Placebo", " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

if (risk_diff == TRUE) {
  adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
  ### to avoid problems with level of trtvar not observed in main domain
  adsl$rrisk_label <- factor(
    paste(adsl[[trtvar]], paste("vs", ctrl_grp)),
    levels = paste(levels(adsl[[trtvar]]), paste("vs", ctrl_grp))
  )
}

# join data together
adae <- adae %>% inner_join(., adsl, by = intersect(names(adae), names(adsl)))

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

# new approach to prevent label problems when treatment group not available in domain dataset
ctrl_grp2 <- paste(ctrl_grp, "vs", ctrl_grp)
ref_path <- c("colspan_trt", " ", "rrisk_label", ctrl_grp2)
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
    # split_cols_by(trtvar, labels_var = "rrisk_label", split_fun = remove_split_levels("Placebo"))
    ### do not use labels_var, but rrisk_label as variable
    ### note updated level in remove_split_levels
    split_cols_by("rrisk_label", split_fun = remove_split_levels(ctrl_grp2))
}

lyt <- lyt %>%
  split_rows_by(
    "OCMQNAM",
    split_label = paste0("OCMQ (", ocmqclass, ")"),
    split_fun = keep_split_levels(ocmqnam_list),
    label_pos = "topleft",
    section_div = c(" "),
    child_labels = "hidden",
    nested = FALSE
  ) %>%
  summarize_row_groups(
    "OCMQNAM",
    cfun = a_freq_j,
    extra_args = extra_args_rr
  ) %>%
  analyze(
    "AEDECOD",
    afun = a_freq_j,
    extra_args = c(extra_args_rr, list(drop_levels = TRUE))
  ) %>%
  append_topleft("  Preferred Term, n (%)")

result <- build_table(lyt, adae, alt_counts_df = adsl)

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
  # result <- sort_at_path(result, c("OCMQNAM"), scorefun = jj_complex_scorefun())
  result <- sort_at_path(
    result,
    c("OCMQNAM", "*", "AEDECOD"),
    scorefun = jj_complex_scorefun()
  )
}

## note : perform this step after sorting, otherwise can result in errors (unable to find children AEDECOD)
## extra step : to remove lines with No data to report: note usage of trim_rows rather than prune_table
## this to ensure the content rows with empty levels are kept

prune_empty_level_tablerow <- function(tt) {
  if (is(tt, "ContentRow")) {
    return(FALSE)
  }
  if (is(tt, "TableRow")) {
    return(all_zero_or_na(tt))
  }
  kids <- tree_children(tt)
  length(kids) == 0
}

result <- result %>% trim_rows(prune_empty_level_tablerow)

## Remove the N=xx column headers for the risk difference columns
result <- remove_col_count(result)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table
################################################################################

tt_to_tlgrtf(result, file = fileid, orientation = "landscape")
