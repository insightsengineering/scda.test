################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsfvit04
## R version:                 4.2.1
## Short Description:         Program to create tsfvit04: Subjects Meeting Specific
##                            On-treatment Hypotension Levels
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      30JAN2024
## Input:                     adsl.RDS, advs.RDS
## Output:                    tsfvit04.rtf
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
library(tern)
library(dplyr)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSFVIT04"
fileid <- write_path(opath, tblid)
titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

popfl <- "SAFFL"
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"

selparamcd <- c("SYSBP", "DIABP")
### as in dataset, order is important for later processing
### not automated, hard coded approach for ease of reading
### ideally the datasets already contain the appropriate case, to ensure units are in proper case
sel_param <- c(
  "Systolic Blood Pressure (mmHg)",
  "Diastolic Blood Pressure (mmHg)"
)
sel_param_case <- c(
  "Systolic blood pressure (mmHg)",
  "Diastolic blood pressure (mmHg)"
)

################################################################################
# Process Data:
################################################################################

adsl <- adsl_jnj %>%
  filter(.data[[popfl]] == "Y") %>%
  select(USUBJID, all_of(c(popfl, trtvar)), SEX, AGEGR1, RACE, ETHNIC)


adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
adsl$rrisk_label <- paste(adsl[[trtvar]], paste("vs", ctrl_grp))

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)

### add On-treatment restriction
filtered_advs <- advs_jnj %>%
  filter(PARAMCD %in% selparamcd) %>%
  ### as this table is using CRIT3 and not a flag like ANL03FL
  ### we need to explicitely apply filter ONTRTFL to restrict to On-treatment
  filter(ONTRTFL == "Y") %>%
  select(
    STUDYID,
    USUBJID,
    PARAMCD,
    PARAM,
    AVALCAT1,
    AVALCA1N,
    AVISIT,
    APOBLFL,
    CRIT3,
    CRIT3FL,
    ONTRTFL
  ) %>%
  inner_join(adsl) %>%
  ### ensure to keep only 1 result per subject, keep N only in case no Y was observed
  arrange(USUBJID, PARAMCD, CRIT3, CRIT3FL) %>%
  group_by(USUBJID, PARAMCD) %>%
  mutate(ncrit3 = n_distinct(CRIT3FL)) %>%
  filter(!(ncrit3 > 1 & CRIT3FL == "N")) %>%
  ## only keep one record
  slice_head(n = 1) %>%
  ungroup()


filtered_advs$PARAM <- factor(
  as.character(filtered_advs$PARAM),
  levels = sel_param,
  labels = sel_param_case
)


### Mapping for CRIT3
### alternative approach to retrieve from metadata iso dataset
xlabel_map <- unique(filtered_advs %>% select(PARAM, CRIT3)) %>%
  rename(label = CRIT3) %>%
  mutate(
    value = "Y",
    label = as.character(label)
  )

################################################################################
# Define layout and build table:
################################################################################

extra_args1 <- list(denom = "n_df", riskdiff = FALSE, .stats = c("n_df"))
extra_args_rr <- list(
  denom = "n_df",
  riskdiff = TRUE,
  method = "wald",
  ref_path = ref_path,
  .stats = c("count_unique_fraction")
)


lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) %>%
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) %>%
  split_cols_by(trtvar) %>%
  split_cols_by("rrisk_header", nested = FALSE) %>%
  split_cols_by(
    trtvar,
    labels_var = "rrisk_label",
    split_fun = remove_split_levels(ctrl_grp)
  ) %>%
  #### this assumes subjects always have both systolic and diastolic parameters
  analyze(
    "CRIT3FL",
    a_freq_j,
    show_labels = "hidden",
    table_names = "CRIT3_N",
    extra_args = extra_args1
  ) %>%
  split_rows_by(
    "PARAM",
    label_pos = "topleft",
    child_labels = "hidden",
    split_label = paste("Blood Pressure (mmHg), n (%)")
  ) %>%
  analyze(
    "CRIT3FL",
    a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(
        val = c("Y"),
        label_map = xlabel_map
      )
    ),
    indent_mod = 1L,
    show_labels = "hidden"
  )

result <- build_table(lyt, filtered_advs, alt_counts_df = adsl)

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

tt_to_tlgrtf(result, file = fileid, orientation = "landscape")
