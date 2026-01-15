################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsfae16.R
## R version:                 4.4.1
## junco Version:             1.0
## Short Description:         Subjects With Treatment-emergent Adverse Events by System
##                            Organ Class and FDA Medical Query (Broad and Narrow)
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      30 Apr 2025
## Input:                     ADSL, ADAEFMQ
## Output:                    TSFAE16.rtf
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
# - Choose whether or not you want to present a combined active treatment column (default=TRUE)
# - Choose whether or not you want to present the risk difference columns (default=TRUE)
# - Choose which risk difference method you would like (default=Wald)
# - Define what the control treatment group is for your study (e.g Placebo)
# - Define how to create combined treatment columns (if required)
################################################################################

tblid <- "TSFAE16"
fileid <- write_path(opath, tblid)
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

trtvar <- "TRT01A"
popfl <- "SAFFL"

combined_colspan_trt <- FALSE
risk_diff <- TRUE
rr_method <- "wald"
ctrl_grp <- "Placebo"

QVAR_CLASS <- "OCMQCLSS"
QVAR_SOC <- "OCMQSOC"
QVAR_NAM <- "OCMQNAM"

if (combined_colspan_trt == TRUE) {
  # Set up levels and label for the required combined columns
  add_combo <- add_combo_facet(
    "Combined",
    label = "Combined",
    levels = c("Xanomeline High Dose", "Xanomeline Low Dose")
  )

  # choose if any facets need to be removed - e.g remove the combined column for ref group
  rm_combo_from_ref <- cond_rm_facets(
    facets = "Combined",
    ancestor_pos = NA,
    value = " ",
    split = "colspan_trt"
  )

  mysplit <- make_split_fun(post = list(add_combo, rm_combo_from_ref))
}

################################################################################
# Process Data:
################################################################################

adsl <- adsl_jnj %>%
  filter(!!rlang::sym(popfl) == "Y") %>%
  select(STUDYID, USUBJID, all_of(trtvar), all_of(popfl))

adsl0 <- adsl

adsl <- rbind(
  adsl0 %>% mutate(!!rlang::sym(QVAR_CLASS) := "Narrow"),
  adsl0 %>% mutate(!!rlang::sym(QVAR_CLASS) := "Broad")
)
### label slightly differs from values in dataset
QVAR_CLASS2 <- "OCMQCLS2"
adsl[[QVAR_CLASS2]] <- paste(adsl[[QVAR_CLASS]], "OCMQs")

adae <- adaeocmq_jnj %>%
  filter(TRTEMFL == "Y") %>%
  select(USUBJID, TRTEMFL, all_of(c(QVAR_CLASS, QVAR_NAM, QVAR_SOC)))

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

# join data together
ae <- adae %>% inner_join(., adsl)

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

if (risk_diff) {
  ## risk difference columns are created by this split function for FMQ table
  # to get risk diff column within FMQclass
  mysplfun1 <- make_split_fun(
    post = list(
      add_combo_facet(
        "RR_difference",
        label = "Risk Difference (%) (95% CI)",
        levels = c("Active Study Agent")
      )
    )
  )
} else {
  mysplfun1 <- NULL
}

# further split process to ensure only the required treatment columns remain in the layout
make_cond_rm_from_map <- function(map, diff_rm = "Placebo") {
  outer_levs <- unique(map[[1]])
  fnlst <- list()
  for (i in seq_along(outer_levs)) {
    inner_levs <- map[map[[1]] == outer_levs[[i]], 2, drop = TRUE]
    fnlst[[i]] <- cond_rm_facets(
      facets = inner_levs,
      ancestor_pos = -1,
      value = outer_levs[i],
      keep_matches = TRUE
    )
  }

  # remove ref group column within RR columns
  rr_rm_cond <- cond_rm_facets(
    facets = diff_rm,
    ancestor_pos = -1,
    value = "RR_difference",
    keep_matches = FALSE
  )

  # update label in risk diff column to include diff from reference in label
  post_relabel <- function(splres, spl, fulldf, .spl_context) {
    inrr <- grepl("difference", tail(.spl_context$value, 1))
    if (inrr) {
      splres$labels <- paste(splres$labels, "vs", diff_rm)
    }
    splres
  }

  make_split_fun(post = c(fnlst, rr_rm_cond, post_relabel))
}

cond_trt_splfun <- make_cond_rm_from_map(colspan_trt_map, diff_rm = ctrl_grp)

################################################################################
# Define layout and build table:
################################################################################

ref_path <- c("colspan_trt", " ", "TRT01A", "Placebo")
extra_args_rr <- list(
  method = rr_method,
  ref_path = ref_path,
  colgroup = "OCMQCLSS",
  .stats = c("count_unique_fraction")
)

lyt <- basic_table(
  top_level_section_div = " ",
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) %>%
  split_cols_by(QVAR_CLASS, labels_var = QVAR_CLASS2) %>%
  split_cols_by("colspan_trt", split_fun = mysplfun1) %>%
  split_cols_by(trtvar, split_fun = cond_trt_splfun)

lyt <- lyt %>%
  split_rows_by(
    QVAR_SOC,
    split_label = "Organ System Class",
    split_fun = trim_levels_in_group(QVAR_NAM),
    label_pos = "topleft",
    section_div = c(" ")
  ) %>%
  summarize_row_groups(
    QVAR_SOC,
    cfun = a_freq_j,
    extra_args = append(extra_args_rr, NULL)
  ) %>%
  analyze(
    QVAR_NAM,
    afun = a_freq_j,
    extra_args = append(extra_args_rr, NULL)
  ) %>%
  append_topleft("  OCMQ, n (%)")

result <- build_table(lyt, ae, alt_counts_df = adsl)


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
  # per guidance of DAS : sorting should be in Broad category
  # this can be achieved by specifying colpath in jj_complex_scorefun
  # see colpath argument -- category Broad is specified here
  # this will result in sorting according first number of first column in Broad category
  # ie sorting on decreasing frequency in Apa group in Broad category
  # you can be more specific (eg specific single column) if you further specify the colpath to trial needs
  cur_scorefun <- jj_complex_scorefun(
    colpath = c(QVAR_CLASS, "Broad")
  )

  result <- sort_at_path(result, c(QVAR_SOC), scorefun = cur_scorefun)
  result <- sort_at_path(
    result,
    c(QVAR_SOC, "*", QVAR_NAM),
    scorefun = cur_scorefun
  )
}

## Remove the N=xx column headers for the risk difference columns
### need to look at non-default level in the col_paths of the table - here at lvl 4 rather than 1
result <- remove_col_count(result, span_label_var = "RR_difference")

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table
################################################################################

tt_to_tlgrtf(result, file = fileid, orientation = "landscape")
