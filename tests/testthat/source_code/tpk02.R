################################################################################
## Original Reporting Effort: Standards
## Program Name:              tpk02
## R version:                 4.4.2
## Short Description:         Program to create tpk02:
##                            [Matrix] [Active Study Agent/Analyte]
##                            Concentrations ([units]) Over Time by
##                            [Body Weight] [Quartiles]; Pharmacokinetics
##                            Analysis Set (Study jjcs - core)
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      20AUG2025
## Input:                     adpc.rds, adsl.rds
## Output:                    tpk02.rtf
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

tblid <- "TPK02"
fileid <- write_path(opath, tblid)
titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")
popfl <- "PKFL"
trtvar <- "TRT01A"

# Flags indicating whether to include certain statistics in the table
add_geometric_mean <- TRUE
add_cv <- TRUE
add_interquartile_range <- TRUE

trt_grps <- c("Xanomeline High Dose", "Xanomeline Low Dose")

for (i in seq_along(trt_grps)) {
  trt_grp <- trt_grps[i]
  path <- paste0(fileid, "part", i)
  
  ##############################################################################
  # Process data:
  ##############################################################################
  
  adsl <- adsl_jnj |>
    filter(.data[[popfl]] == "Y") |>
    filter(.data[[trtvar]] == trt_grp) |>
    select(USUBJID, all_of(trtvar), WGTGR1, PKFL) |>
    mutate(colspan = "[Body Weight (kg)] [Quartiles]")
  
  adpc <- adpc_jnj |>
    filter(PARAMCD == "XAN") |>
    filter(.data[[trtvar]] == trt_grp) |>
    select(USUBJID, all_of(trtvar), AVISIT, ATPT, AVAL) |>
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
    split_cols_by("WGTGR1") |>
    
    split_rows_by(
      var = "AVISIT_ATPT",
      split_label = "Time Point",
      label_pos = "topleft",
      section_div = " "
    ) |>
    
    analyze(
      vars = "AVAL",
      afun = a_summary,
      extra_args = list(
        .stats = c(
          "n",
          "mean_sd",
          "median",
          if (add_geometric_mean) "geom_mean" else NULL, 
          "range",
          if (add_cv) "cv" else NULL,
          if (add_interquartile_range) "quantiles" else NULL
        ),
        .labels = c(
          n = "N",
          mean_sd = "Mean (SD)",
          median = "Median",
          geom_mean = "Geometric mean",
          range = "Min, max",
          cv = "CV (%)",
          quantiles = "Interquartile range"
        ),
        .formats = c(
          mean_sd = jjcsformat_xx("xx.xx (xx.xx)"),
          median = jjcsformat_xx("xx.xx"),
          geom_mean = jjcsformat_xx("xx.xx"),
          range = jjcsformat_xx("xx.xx, xx.xx"),
          cv = jjcsformat_xx("xx.x"),
          quantiles = jjcsformat_xx("xx.xx, xx.xx")
        ),
        .indent_mods = c(
          n = 0,
          mean_sd = 1,
          median = 1,
          geom_mean = 1,
          range = 1,
          cv = 1,
          quantiles = 1
        )
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
  
  tt_to_tlgrtf(result, file = path, orientation = "portrait")
}
