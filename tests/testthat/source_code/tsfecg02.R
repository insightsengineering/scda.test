################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsfecg02
## R version:                 4.2.1
## Short Description:         Program to create tsfecg02: Categorized Corrected
##                            QT Interval Values Over Time
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      30JAN2024
## Input:                     adsl.RDS, adeg.RDS
## Output:                    tsfecg02.rtf
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

tblid <- "TSFECG02"
fileid <- write_path(opath, tblid)
titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

popfl <- "SAFFL"
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"

selparamcd <- c("QTC", "QTCBAG", "QTCFAG")
selvisit <- c(
  "Baseline",
  "Month 1",
  "Month 3",
  "Month 6",
  "Month 9",
  "Month 12"
)

################################################################################
# Process Data:
################################################################################

adsl <- adsl_jnj %>%
  filter(.data[[popfl]] == "Y") %>%
  select(
    USUBJID,
    all_of(c(popfl, trtvar)),
    SEX_DECODE,
    AGEGR1,
    RACE_DECODE,
    ETHNIC_DECODE
  )

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)


filtered_adeg <- adeg_jnj %>%
  filter(PARAMCD %in% selparamcd) %>%
  filter(AVISIT %in% selvisit) %>%
  filter(ANL02FL == "Y") %>%
  filter(!is.na(AVALCAT1)) %>%
  select(
    STUDYID,
    USUBJID,
    PARAMCD,
    PARAM,
    AVALCAT1,
    AVALCA1N,
    AVISIT,
    ANL02FL,
    APOBLFL,
    ABLFL,
    ONTRTFL
  ) %>%
  inner_join(adsl)

## main analysis variable is AVALCAT1 : these have the same levels for all selected parameters
## no need to create an map dataframe for usage in layout

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)

################################################################################
# Define layout and build table:
################################################################################

extra_args_rr <- list(
  method = "wald",
  denom = "n_df",
  .stats = c("denom", "count_unique_fraction")
)
# extra_args_rr2 <- list(method = "wald", denom = "n_df", .stats = c("denom",  "count_unique_denom_fraction")) # version with explicit denominator (usefull for testing purpose)

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) %>%
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) %>%
  split_cols_by(trtvar) %>%
  split_rows_by(
    "PARAM",
    label_pos = "topleft",
    child_labels = "default",
    split_label = "QTc Interval",
    section_div = " ",
    ## ensure only selected params are included
    split_fun = drop_split_levels
  ) %>%
  split_rows_by(
    "AVISIT",
    label_pos = "topleft",
    child_labels = "default",
    split_label = "Study Visit",
    section_div = " ",
    ## ensure only the selected visits are included
    split_fun = drop_split_levels
  ) %>%
  analyze(
    "AVALCAT1",
    a_freq_j,
    extra_args = extra_args_rr,
    show_labels = "hidden",
    indent_mod = 0L
  ) %>%
  append_topleft(c("   Criteria, n (%)"))


result <- build_table(lyt, filtered_adeg, alt_counts_df = adsl)

################################################################################
# Post-Processing:
# - remove unwanted colcounts
################################################################################

result <- remove_col_count(result)


################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, titles)

################################################################################
# Convert to tbl file and output table
################################################################################

tt_to_tlgrtf(result, file = fileid)
