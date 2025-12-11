################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsiex06.R
## R version:                 4.2.1
## junco version:             1.0
## Short Description:         Program to create tsiex06: Subjects Who Received Study
##                            Treatment by Time Point
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      29 Jan 2024
## Input:                     ADSL, ADEX
## Output:                    TSIEX06.rtf
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

tblid <- "TSIEX06"
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
  select(USUBJID, all_of(trtvar), all_of(popfl))

adex <- adex_jnj %>%
  filter(AOCCUR == "Y" & !grepl("UNSCHEDULED", AVISIT, ignore.case = TRUE)) %>%
  select(USUBJID, AOCCUR, AVISIT, AVISITN)

# Convert AVISIT to sentence case and apply levels to maintain ordering
avisit_levs <- stringr::str_to_sentence(levels(adex$AVISIT))
adex$AVISIT <- stringr::str_to_sentence(adex$AVISIT)
adex$AVISIT <- factor(adex$AVISIT, levels = avisit_levs)

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
  tern::count_occurrences(
    "AVISIT",
    .stats = "count_fraction_fixed_dp",
    .formats = c("count_fraction_fixed_dp" = jjcsformat_count_fraction),
    var_labels = "Subjects receiving study treatment, n (%)",
    show_labels = "visible"
  )

result <- build_table(lyt, ex, alt_counts_df = adsl)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table
################################################################################
tt_to_tlgrtf(result, file = fileid, orientation = "portrait")
