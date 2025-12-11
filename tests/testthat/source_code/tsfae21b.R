###############################################################################################
## Original Reporting Effort: Standards
## Program Name: tsfae21b.R
## R Version: 4.2.1
## junco Version: 1.0
## Short Description: Program to create tsfae21b: TEAEs by toxicity (SOC / PT) - Variant 1
## Author:                    Johnson & Johnson Innovative Medicine
## Date: 26 Feb 2024
## Input: ADSL, ADAE.
## Output: TSFAE21b.rtf
## Remarks: Template R script version using rtables framework
##
## Modification History:
## Rev #: 1
## Modified By:
## Reporting effort:
## Date:
## Description:
###############################################################################################

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
# - Define column widths to help with desired page splitting
# - Option to exclude Grade 5 columns if there are none (default = TRUE but set to FALSE if you want this column regardless)
################################################################################

tblid <- "TSFAE21b"
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

exclude_G5_if_none <- TRUE

################################################################################
# Process Data:
################################################################################

adsl <- adsl_jnj %>%
  filter(!!rlang::sym(popfl) == "Y") %>%
  select(STUDYID, USUBJID, all_of(trtvar), all_of(popfl))

adae <- adae_jnj %>%
  filter(TRTEMFL == "Y")

# Take maximum toxicity - per PT
adaemaxpt <- adae %>%
  filter(AETOXGR %in% c("1", "2", "3", "4", "5")) %>%
  arrange(USUBJID, AEBODSYS, AEDECOD, AETOXGRN) %>%
  group_by(USUBJID, AEBODSYS, AEDECOD) %>%
  slice_tail() %>%
  ungroup()

# Take maximum toxicity - per SOC
adaemaxsoc <- adae %>%
  filter(AETOXGR %in% c("1", "2", "3", "4", "5")) %>%
  mutate(AEBODSYSx = AEBODSYS) %>%
  arrange(USUBJID, AEBODSYS, AETOXGRN) %>%
  group_by(USUBJID, AEBODSYS) %>%
  slice_tail() %>%
  ungroup() %>%
  select(USUBJID, AEBODSYS, AETOXGR, AEBODSYSx)

# Merge back in an create a new SOC variable that is only populated for max toxicity SOC rows
adaemax <- left_join(
  adaemaxpt,
  adaemaxsoc,
  by = c("USUBJID", "AEBODSYS", "AETOXGR")
)

# Add total
adaetot <- adae %>%
  mutate(
    AETOXGR = "Total",
    AEBODSYSx = AEBODSYS
  ) %>%
  arrange(USUBJID, AEBODSYS, AEDECOD) %>%
  group_by(USUBJID, AEBODSYS, AEDECOD) %>%
  slice(1) %>%
  ungroup()

# Set data together
adaeall <- bind_rows(adaemax, adaetot) %>%
  mutate(
    AETOXGR = factor(
      as.character(AETOXGR),
      levels = c("Total", "1", "2", "3", "4", "5")
    )
  ) %>%
  select(USUBJID, TRTEMFL, AETOXGR, AEBODSYS, AEBODSYSx, AEDECOD)

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == "Placebo", " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = "Placebo",
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

# join data together
ae <- adaeall %>% inner_join(., adsl, by = c("USUBJID"))

if (length(adae$TRTEMFL) == 0) {
  ae <- adaeall %>% right_join(., adsl, by = c("USUBJID"))
}

ae$spanheader <- factor(
  ifelse(ae$AETOXGR == "Total", " ", "Toxicity Grade"),
  levels = c(" ", "Toxicity Grade")
)

adsl1 <- adsl %>%
  mutate(AETOXGR = "Total")

adsl <- adsl1 %>%
  mutate(
    AETOXGR = factor(
      as.character(AETOXGR),
      levels = c("Total", "1", "2", "3", "4", "5")
    )
  )
adsl$spanheader <- factor(
  ifelse(adsl$AETOXGR == "Total", " ", "Toxicity Grade"),
  levels = c(" ", "Toxicity Grade")
)


grademap <- data.frame(
  spanheader = c(
    " ",
    "Toxicity Grade",
    "Toxicity Grade",
    "Toxicity Grade",
    "Toxicity Grade",
    "Toxicity Grade"
  ),
  AETOXGR = c("Total", "1", "2", "3", "4", "5"),
  stringsAsFactors = FALSE
)

# If there are no grade 5 and user has asked to exclude column then update factors
anyG5 <- ae %>%
  filter(AETOXGR == "5")

if (exclude_G5_if_none == TRUE && length(anyG5$AETOXGR) == 0) {
  grademap <- data.frame(
    spanheader = c(
      " ",
      "Toxicity Grade",
      "Toxicity Grade",
      "Toxicity Grade",
      "Toxicity Grade"
    ),
    AETOXGR = c("Total", "1", "2", "3", "4"),
    stringsAsFactors = FALSE
  )
}

################################################################################
# Define layout and build table:
################################################################################

extra_args_1 <- list(
  denom = "N_colgroup",
  .stats = c("count_unique_fraction"),
  colgroup = trtvar
)


lyt <- basic_table(
  top_level_section_div = " ",
  colcount_format = "N=xx"
) %>%
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  )

if (combined_colspan_trt == TRUE) {
  lyt <- lyt %>%
    split_cols_by(trtvar, split_fun = mysplit, show_colcounts = TRUE)
} else {
  lyt <- lyt %>%
    split_cols_by(trtvar, show_colcounts = TRUE)
}

lyt <- lyt %>%
  split_cols_by(
    "spanheader",
    split_fun = trim_levels_to_map(map = grademap)
  ) %>%
  split_cols_by("AETOXGR", show_colcounts = FALSE) %>%
  analyze(
    "TRTEMFL",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = append(
      extra_args_1,
      list(
        label = "Subjects with >=1 AE",
        val = "Y",
        restr_columns = "Total"
      )
    )
  ) %>%
  split_rows_by(
    "AEBODSYS",
    split_label = "System Organ Class",
    split_fun = trim_levels_in_group("AEDECOD"),
    label_pos = "topleft",
    section_div = c(" ")
  ) %>%
  summarize_row_groups(
    "AEBODSYSx",
    cfun = a_freq_j,
    extra_args = extra_args_1
  ) %>%
  analyze("AEDECOD", afun = a_freq_j, extra_args = extra_args_1) %>%
  append_topleft("  Preferred Term, n (%)")

result <- build_table(lyt, ae, alt_counts_df = adsl)

#########################################################################################
# Post-Processing step to sort by descending count on chosen active treatment columns.
# Default is the last treatment (inc. Combined if applicable) under the active treatment
# spanning header (defaulted to colspan_trt variable).
# For this table we can use a defined colpath so it takes the appropriate sub-column ("Total")
# for the last active treatment group/combined and use this for its sort order.
# If you only have 1 active treatment arm, consider using jj_complex_scorefun(spanningheadercolvar = NA, usefirstcol = TRUE)
# See function documentation for jj_complex_scorefun should your require a different sorting behavior.
#########################################################################################

# col_paths_summary(result)

if (length(adae$TRTEMFL) != 0) {
  result <- sort_at_path(
    result,
    c("root", "AEBODSYS"),
    scorefun = jj_complex_scorefun(
      colpath = c(
        "colspan_trt",
        "Active Study Agent",
        trtvar,
        "Combined",
        "spanheader",
        " ",
        "AETOXGR",
        "Total"
      )
    )
  )
  result <- sort_at_path(
    result,
    c("root", "AEBODSYS", "*", "AEDECOD"),
    scorefun = jj_complex_scorefun(
      colpath = c(
        "colspan_trt",
        "Active Study Agent",
        trtvar,
        "Combined",
        "spanheader",
        " ",
        "AETOXGR",
        "Total"
      )
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

tt_to_tlgrtf( 
  result,
  file = fileid,
  orientation = "landscape",
  nosplitin = list(cols = c(trtvar))
)
