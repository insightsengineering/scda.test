################################################################################
## Original Reporting Effort: Standards
## Program Name:              tpk01b
## R version:                 4.4.2
## Short Description:         Program to create tpk01b:
##                            [Matrix] [Active Study Agent/Analyte]
##                            Concentrations ([units]) Over Time;
##                            Pharmacokinetics Analysis Set (Study jjcs - core)
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      19AUG2025
## Input:                     adpc.rds, adsl.rds
## Output:                    tpk01b.rtf
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
library(rtables)
library(tern)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TPK01b"
fileid <- write_path(opath, tblid)
titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")
popfl <- "PKFL"
trtvar <- "TRT01A"

trt_grps <- c("Xanomeline High Dose", "Xanomeline Low Dose")

for (i in seq_along(trt_grps)) {
  trt_grp <- trt_grps[i]
  path <- paste0(fileid, "part", i)
  
  ##############################################################################
  # Process data:
  ##############################################################################
  
  adsl <- adsl_jnj |>
    filter(.data[[popfl]] == "Y") |>
    select(USUBJID, all_of(trtvar), PKFL) |>
    filter(.data[[trtvar]] == trt_grp) |>
    mutate(colspan = "Observed")
  
  adpc <- adpc_jnj |>
    filter(PARAMCD == "XAN") |>
    select(USUBJID, all_of(trtvar), AVISIT, ATPT, AVAL) |>
    filter(.data[[trtvar]] == trt_grp) |>
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
  
  ##############################################################################
  # Define layout and build table:
  ##############################################################################
  
  lyt <- basic_table() |>
    split_cols_by(
      var = trtvar,
      show_colcounts = TRUE,
      colcount_format = "N=xx",
      split_fun = drop_split_levels
    ) |>
    
    split_cols_by("colspan") |>
    
    split_rows_by(
      var = "AVISIT_ATPT",
      split_label = "Time Point",
      label_pos = "topleft",
      child_labels = "hidden"
    ) |>
    
    analyze_vars_in_cols(
      vars = "AVAL",
      .stats = c(
        "n",
        "mean",
        "sd",
        "median",
        "min",
        "max",
        "cv",
        "geom_mean"
      ),
      .labels = c(
        n = "N",
        mean = "Mean",
        sd = "SD",
        median = "Med",
        min = "Min",
        max = "Max",
        cv = "% CV",
        geom_mean = "Geometric Mean"
      ),
      .formats = c(
        n = jjcsformat_xx("xx"),
        mean = jjcsformat_xx("xx.xx"),
        sd = jjcsformat_xx("xx.xx"),
        median = jjcsformat_xx("xx.xx"),
        min = jjcsformat_xx("xx.xx"),
        max = jjcsformat_xx("xx.xx"),
        cv = jjcsformat_xx("xx.x"),
        geom_mean = jjcsformat_xx("xx.xx")
      )
    )
  
  result <- build_table(lyt, df = adpc, alt_counts_df = adsl)
  
  ##############################################################################
  # Add titles and footnotes:
  ##############################################################################
  
  result <- set_titles(result, titles)
  
  ##############################################################################
  # Convert to tbl file and output table:
  ##############################################################################
  
  tt_to_tlgrtf(result, file = path, orientation = "landscape")
}
