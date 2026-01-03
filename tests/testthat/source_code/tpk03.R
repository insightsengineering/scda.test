################################################################################
## Original Reporting Effort: Standards
## Program Name:              tpk03
## R version:                 4.4.2
## Short Description:         Program to create tpk03:
##                            Subjects With [Matrix]
##                            [Active Study Agent/Analyte] Concentrations Below
##                            the Lowest Quantification Level ([LLOQ/LLOQxMRD
##                            and units]) Over Time; Pharmacokinetics Analysis
##                            Set (Study jjcs - core)
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      22AUG2025
## Input:                     adpc.rds, adsl.rds
## Output:                    tpk03.rtf
## Remarks:
##
## Modification History:
##  Rev #:
##  Modified By:
##  Reporting Effort:
##  Date:
##  Description:
################################################################################

################################################################################
# Prep environment:
################################################################################

library(envsetup)
library(dplyr)
library(forcats)
library(rtables)
library(tern)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TPK03"
fileid <- write_path(opath, tblid)
titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)
popfl <- "PKFL"
trtvar <- "TRT01A"

################################################################################
# Process data:
################################################################################

adsl <- adsl_jnj |>
  filter(.data[[popfl]] == "Y") |>
  select(USUBJID, all_of(trtvar), PKFL) |>
  # Drop the control group
  filter(.data[[trtvar]] != "Placebo") |>
  mutate({{ trtvar }} := fct_drop(.data[[trtvar]])) |>
  mutate(colspan_trt = "Active Study Agent")

adpc <- adpc_jnj |>
  filter(PARAMCD == "XAN") |>
  select(USUBJID, all_of(trtvar), AVISIT, ATPT, CRIT1FL) |>
  inner_join(adsl) |>
  # Concatenate and reorder time points
  mutate(AVISIT_ATPT = factor(paste(AVISIT, ATPT, sep = " - "), levels = c(
    "Day 1 - Pre-dose",
    "Day 1 - 5 Min Post-dose",
    "Day 1 - 30 Min Post-dose",
    "Day 1 - 1h Post-dose",
    "Day 1 - 1.5h Post-dose",
    "Day 1 - 2h Post-dose",
    "Day 1 - 4h Post-dose",
    "Day 1 - 0-6h Post-dose",
    "Day 1 - 6h Post-dose",
    "Day 1 - 8h Post-dose",
    "Day 1 - 6-12h Post-dose",
    "Day 1 - 12h Post-dose",
    "Day 1 - 16h Post-dose",
    "Day 1 - 12-24h Post-dose",
    "Day 2 - 24h Post-dose",
    "Day 2 - Pre-dose",
    "Day 2 - 36h Post-dose",
    "Day 2 - 24-48h Post-dose",
    "Day 3 - 48h Post-dose",
    "Day 3 - Pre-dose"
  )))

################################################################################
# Define layout and build table:
################################################################################

lyt <- basic_table() |>
  split_cols_by(
    var = "colspan_trt",
    split_fun = drop_split_levels
  ) |>
  split_cols_by(
    var = trtvar,
    show_colcounts = TRUE,
    colcount_format = "N=xx",
    split_fun = add_overall_level("Combined", first = FALSE)
  ) |>
  split_rows_by(
    var = "AVISIT_ATPT",
    split_label = "Time Point",
    label_pos = "topleft",
    section_div = " "
  ) |>
  analyze(
    vars = "PKFL",
    show_labels = "hidden",
    afun = a_freq_j,
    extra_args = list(
      val = "Y",
      label = "N",
      .stats = "count_unique"
    )
  ) |>
  analyze(
    vars = "CRIT1FL",
    show_labels = "hidden",
    afun = a_freq_j,
    extra_args = list(
      val = "Y",
      label = "Number of subjects with values below LLOQxMRD, n (%)",
      .stats = "count_unique_fraction",
      .indent_mods = 1
    )
  )

result <- build_table(lyt, df = adpc, alt_counts_df = adsl)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, titles)

################################################################################
# Convert to tbl file and output table:
################################################################################

tt_to_tlgrtf(result, file = fileid, orientation = "portrait")
