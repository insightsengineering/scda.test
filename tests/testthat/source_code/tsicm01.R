################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsicm01.R
## R version:                 4.2.1
## junco version:             1.0
## Short Description:         Program to create tsicm01: Concomitant Medications by
##                            ATC Level X/Standardized Medication Name - Variant 1
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      13 Nov 2023
## Input:                     ADSL, ADCM.
## Output:                    TSICM01.rtf
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
# - Define dynamic ATC Variable
# - Define dynamic ATC Level
# - Choose whether or not you want to present a combined active treatment column (default=TRUE)
# - Define how to create combined treatment columns (if required)
################################################################################

tblid <- "TSICM01"
fileid <- write_path(opath, tblid)
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

trtvar <- "TRT01A"
popfl <- "SAFFL"

atcvar <- "CMLVL1"
atclevel <- "1"

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

adcm <- adcm_jnj %>%
  filter(ONTRTFL == "Y") %>%
  select(USUBJID, ONTRTFL, all_of(atcvar), CMDECOD)

# Convert medications to sentence case
adcm[[atcvar]] <- as.factor(stringr::str_to_sentence(adcm[[atcvar]]))
adcm$CMDECOD <- as.factor(stringr::str_to_sentence(adcm$CMDECOD))

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == "Placebo", " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

# join data together
cm <- adcm %>% inner_join(., adsl, by = c("USUBJID"))

if (length(adcm$ONTRTFL) == 0) {
  cm <- adcm %>% right_join(., adsl, by = c("USUBJID"))
}

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

extra_args_1 <- list(.stats = "count_unique_fraction")

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
  add_overall_col("Total") %>%
  analyze(
    "ONTRTFL",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = append(
      extra_args_1,
      list(label = "Subjects with >=1 concomitant medication")
    )
  ) %>%
  split_rows_by(
    atcvar,
    child_labels = "hidden",
    split_label = paste0("ATC Level ", atclevel),
    label_pos = "topleft",
    split_fun = trim_levels_in_group("CMDECOD"),
    section_div = c(" "),
    indent_mod = 0L
  ) %>%
  summarize_row_groups(atcvar, cfun = a_freq_j, extra_args = extra_args_1) %>%
  analyze("CMDECOD", afun = a_freq_j, extra_args = extra_args_1) %>%
  append_topleft("  Standardized Medication Name")

result <- build_table(lyt, cm, alt_counts_df = adsl)

# If there is no data remove top row and display "No data to display" text
if (length(adcm$ONTRTFL) == 0) {
  result <- safe_prune_table(
    result,
    prune_func = remove_rows(
      removerowtext = "Subjects with >=1 concomitant medication"
    )
  )
}

#########################################################################################
# Post-Processing step to sort by descending count on total column:
#########################################################################################

if (length(adcm$ONTRTFL) != 0) {
  result <- sort_at_path(
    result,
    c("root", atcvar),
    scorefun = jj_complex_scorefun(colpath = "Total")
  )
  result <- sort_at_path(
    result,
    c("root", atcvar, "*", "CMDECOD"),
    scorefun = jj_complex_scorefun(colpath = "Total")
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
