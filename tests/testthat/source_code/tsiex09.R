################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsiex09.R
## R version:                 4.2.1
## junco version:             1.0
## Short Description:         Program to create tsiex09: Distribution of Subjects by
##                            Study Treatment Lot
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      29 Jan 2024
## Input:                     ADSL, ADEX
## Output:                    TSIEX09.rtf
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

tblid <- "TSIEX09"
fileid <- write_path(opath, tblid)
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


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

adex <- adex_jnj %>%
  select(USUBJID, ATRT, EXLOT)

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == "Placebo", " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

# join data together
ex <- adex %>%
  inner_join(., adsl, by = c("USUBJID")) %>%
  mutate(ATRT = stringr::str_to_sentence(ATRT)) %>%
  mutate(trttxt = paste0(ATRT, " lots"))

ex$trttxt2 <- factor(ex$trttxt)

# Puts placebo last - this line can be removed if you do not have a placebo treatment arm
ex$trttxt2 <- forcats::fct_relevel(ex$trttxt2, "Placebo lots", after = Inf)

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

extra_args1 <- list(
  .stats = "count_unique_fraction",
  denom = "n_parentdf",
  # trick to get 0 for Placebo column for non Placebo lots
  denom_by = "STUDYID"
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
  # trick to get 0 for Placebo column for non Placebo lots
  # we do not want to show this split_rows group label - therefor set child_labels to hidden
  split_rows_by("STUDYID", child_labels = "hidden") %>%
  split_rows_by(
    "trttxt2",
    split_label = "Study Treatment",
    split_fun = trim_levels_in_group("STUDYID"),
    label_pos = "topleft",
    indent_mod = 0L,
    section_div = c(" ")
  ) %>%
  analyze(
    "EXLOT",
    afun = a_freq_j,
    extra_args = extra_args1,
    indent_mod = 0L,
    show_labels = "hidden"
  ) %>%
  append_topleft("  Lot, n (%)")

result <- build_table(lyt, ex, alt_counts_df = adsl)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table
################################################################################
tt_to_tlgrtf(result, file = fileid, orientation = "portrait")
