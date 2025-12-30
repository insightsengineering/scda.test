################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsiex08.R
## R version:                 4.2.1
## junco version:             1.0
## Short Description:         Program to create tsiex08: Incidence and Reason for Dose
##                            Modification
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      16 Jan 2024
## Input:                     ADSL, ADEX
## Output:                    TSIEX08.rtf
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

tblid <- "TSIEX08"
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

# Read in required data
adsl <- adsl_jnj %>%
  filter(!!rlang::sym(popfl) == "Y") %>%
  select(STUDYID, USUBJID, all_of(trtvar), all_of(popfl))

adex1 <- adex_jnj %>%
  filter(!grepl("UNSCHEDULED", AVISIT, ignore.case = TRUE))

# Convert AVISIT to sentence case and apply levels to maintain ordering
avisit_levs <- stringr::str_to_sentence(levels(adex1$AVISIT))
adex1$AVISIT <- stringr::str_to_sentence(adex1$AVISIT)
adex1$AVISIT <- factor(adex1$AVISIT, levels = avisit_levs)

adex2 <- adex_jnj %>%
  mutate(AVISIT = "Overall")

adex_ <- bind_rows(adex1, adex2)

adex1$AVISIT <- droplevels(adex1$AVISIT)
adex_$AVISIT <- factor(
  adex_$AVISIT,
  levels = c("Overall", levels(adex1$AVISIT))
)

adex <- adex_ %>%
  select(USUBJID, ACAT1, ACAT2, AREASOC, AADJ, AVISIT, AVISITN)

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == "Placebo", " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

# join data together
ex <- adex %>% inner_join(., adsl, by = c("USUBJID"))

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
# Note :
# variables AREASOC and AADJ contain missing values, these are removed inside a_freq_j when using denom = "n_df"
# either use n_parentdf and denom_by = AVISIT to get proper denominator
# alternative would be to use ex <- df_explicit_na(ex, omit_columns = "colspan_trt", na_level = "<Missing>") and
# and have denom = "n_df" in extra_args1 and
# add excl_levels = "<Missing>" to the extra_args in the calls to these 2 variables
# excl_levels cannot be used in the overall extra_args1 as val and excl_levels cannot be used together

extra_args1 <- list(
  .stats = "count_unique_fraction",
  denom = "n_parentdf",
  denom_by = "AVISIT"
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
  split_rows_by(
    "AVISIT",
    split_label = "Time Point",
    split_fun = trim_levels_in_group("STUDYID"),
    label_pos = "topleft",
    indent_mod = 0L,
    section_div = c(" ")
  ) %>%
  summarize_row_groups(
    "AVISIT",
    cfun = a_freq_j,
    extra_args = list(.stats = "n_df"),
    indent_mod = 0L
  ) %>%
  analyze(
    "ACAT2",
    afun = a_freq_j,
    extra_args = append(
      extra_args1,
      list(label = "Dose not administered", val = "Dose not administered")
    ),
    indent_mod = 0L,
    show_labels = "hidden"
  ) %>%
  analyze(
    "AREASOC",
    afun = a_freq_j,
    extra_args = extra_args1,
    show_labels = "hidden",
    indent_mod = 1L
  ) %>%
  analyze(
    "ACAT1",
    afun = a_freq_j,
    extra_args = append(
      extra_args1,
      list(label = "Dose adjusted", val = "Dose adjusted")
    ),
    indent_mod = 0L,
    show_labels = "hidden"
  ) %>%
  analyze(
    "AADJ",
    afun = a_freq_j,
    extra_args = extra_args1,
    show_labels = "hidden",
    indent_mod = 1L
  ) %>%
  append_topleft("  Dose Modification, n (%)")

result <- build_table(lyt, ex, alt_counts_df = adsl)

################################################################################
# Prune table to only keep those that have >0 counts in any treatment column specified
################################################################################
more_than_zero <- has_count_in_any_col(
  atleast = 1,
  col_names = c(
    "Active Study Agent.Xanomeline High Dose",
    "Active Study Agent.Xanomeline Low Dose",
    "Active Study Agent.Combined",
    " .Placebo"
  )
)

result <- safe_prune_table(result, keep_rows(more_than_zero))

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table
################################################################################
tt_to_tlgrtf(result, file = fileid, orientation = "portrait")
