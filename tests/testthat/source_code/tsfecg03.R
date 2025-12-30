################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsfecg03
## R version:                 4.2.1
## Short Description:         Program to create tsfecg03: Categorized Change From
##                            Baseline to Maximum On-treatment Corrected QT Interval
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      30JAN2024
## Input:                     adsl.RDS, adeg.RDS
## Output:                    tsfecg03.rtf
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

tblid <- "TSFECG03"
fileid <- write_path(opath, tblid)
titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

popfl <- "SAFFL"
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"

ad_domain <- "adeg"

selvisit <- c(
  "Baseline",
  "Month 1",
  "Month 3",
  "Month 6",
  "Month 9",
  "Month 12",
  "Month 15",
  "Month 18",
  "Month 24"
)

## selection of QTC parameters
selparamcd <- c("QTCFAG", "QTCBAG", "QTCS", "QTCLAG")

################################################################################
# initial read of data
################################################################################

adeg_complete <- adeg_jnj

### available QTC parameters in study
selparamcd <- intersect(selparamcd, unique(adeg_complete$PARAMCD))


catvar <- "CHGCAT1"
## all parameters have the same levels for CHGCAT1 -- there is no need to create a map dataframe

################################################################################
# Process Data:
################################################################################

adsl <- adsl_jnj %>%
  filter(.data[[popfl]] == "Y") %>%
  select(USUBJID, all_of(c(popfl, trtvar)))


adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
adsl$rrisk_label <- paste(adsl[[trtvar]], paste("vs", ctrl_grp))


adeg <- adeg_complete %>%
  filter(PARAMCD %in% selparamcd) %>%
  # filter(AVISIT %in% selvisit) %>%
  ### Maximum On-treatment
  ### note: by filter ANL03FL, this table is restricted to On-treatment values, per definition of ANL03FL
  ### therefor, no need to add ONTRTFL in filter
  ### if derivation of ANL03FL is not restricted to ONTRTFL records, adding ONTRTFL here will not give the correct answer either
  ### as mixing worst with other period is not giving the proper selection !!!
  filter(ANL03FL == "Y") %>%
  select(
    USUBJID,
    ONTRTFL,
    TRTEMFL,
    PARAM,
    PARAMCD,
    AVISITN,
    AVISIT,
    AVAL,
    BASE,
    CHG,
    CRIT1,
    CRIT1FL,
    CRIT2,
    CRIT2FL,
    all_of(catvar),
    ONTRTFL,
    TRTEMFL,
    ANL01FL,
    ANL02FL
  ) %>%
  inner_join(., adsl)


check1 <- adeg %>%
  group_by(TRT01A, PARAMCD, AVISIT) %>%
  summarize(n = n_distinct(USUBJID))

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


lyt <- basic_table(show_colcounts = TRUE, colcount_format = "N=xx") %>%
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) %>%
  split_cols_by(
    trtvar
    # , split_fun = add_combo_levels(combodf)
  ) %>%
  ### if risk diff columns are wanted - re-enable next 2 split_cols_by lines
  # split_cols_by("rrisk_header", nested = FALSE) %>%
  # split_cols_by(trtvar, labels_var = "rrisk_label",
  #               split_fun = remove_split_levels(ctrl_grp))  %>%

  split_rows_by(
    "PARAM",
    split_label = "QTc Interval",
    label_pos = "topleft",
    split_fun = drop_split_levels,
    section_div = " "
  ) %>%
  analyze(
    c("CHGCAT1"),
    a_freq_j,
    extra_args = extra_args_rr,
    show_labels = "hidden",
    indent_mod = 1L
  ) %>%
  append_topleft("   Criteria, n (%)")

result <- build_table(lyt, adeg, alt_counts_df = adsl)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, titles)

################################################################################
# Convert to tbl file and output table
################################################################################

tt_to_tlgrtf(result, file = fileid)
