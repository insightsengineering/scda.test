################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsiex07.R
## R version:                 4.2.1
## junco version:             1.0
## Short Description:         Program to create tsiex07: Dose Levels Over Time
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      15 Jan 2024
## Input:                     ADSL, ADEX
## Output:                    TSIEX07.rtf
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
# - Define the column ordering of the dose levels
################################################################################

tblid <- "TSIEX07"
fileid <- write_path(opath, tblid)
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


trtvar <- "TRT01A"
popfl <- "SAFFL"

dose_order <- c("60", "120", "180", "240", "300", "480", "720", "960", "Total")

################################################################################
# Process Data:
################################################################################

# Read in required data
adsl <- adsl_jnj %>%
  filter(!!rlang::sym(popfl) == "Y" & !!rlang::sym(trtvar) != "Placebo") %>%
  select(USUBJID, all_of(trtvar), all_of(popfl))

adex <- adex_jnj %>%
  filter(!grepl("UNSCHEDULED", AVISIT, ignore.case = TRUE)) %>%
  filter(!is.na(ADOSE)) %>%
  mutate(ADOSEC = as.factor(ADOSE)) %>%
  select(USUBJID, ADOSEC, AVISIT, AVISITN)

# Convert AVISIT to sentence case and apply levels to maintain ordering
avisit_levs <- stringr::str_to_sentence(levels(adex$AVISIT))
adex$AVISIT <- stringr::str_to_sentence(adex$AVISIT)
adex$AVISIT <- factor(adex$AVISIT, levels = avisit_levs)

# Create Total column manually as we cannot use the table layout option since patients can
# appear in multiple dose level columns

extot_ <- adex %>%
  group_by(USUBJID, AVISIT) %>%
  slice(1) %>%
  mutate(ADOSEC = "Total") %>%
  ungroup()

extot <- bind_rows(adex, extot_)

# join data together
ex <- extot %>% inner_join(., adsl, by = c("USUBJID"))

ex$colspan_trt <- factor(
  ifelse(ex$ADOSEC == "Total", " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

ex$ADOSEC <- factor(ex$ADOSEC, levels = dose_order)

# Create dataset to be used for column counts
adex_unique <- ex %>%
  group_by(USUBJID, ADOSEC) %>%
  slice(1) %>%
  ungroup()

################################################################################
# Define layout and build table:
################################################################################

lyt <- rtables::basic_table(
  top_level_section_div = " ",
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) %>%
  split_cols_by("colspan_trt", split_fun = trim_levels_in_group("ADOSEC")) %>%
  split_cols_by("ADOSEC") %>%
  tern::count_occurrences(
    "AVISIT",
    .stats = "count_fraction_fixed_dp",
    .formats = c("count_fraction_fixed_dp" = jjcsformat_count_fraction),
    var_labels = " ",
    show_labels = "hidden"
  ) %>%
  append_topleft("Time Point, n (%)")

result <- build_table(lyt, ex, alt_counts_df = adex_unique)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table
################################################################################
tt_to_tlgrtf(result, file = fileid, orientation = "landscape")
