################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsfvit02
## R version:                 4.2.1
## Short Description:         Program to create tsfvit02: Subjects With Maximum
##                            On-treatment Systolic Blood Pressure by Category of Blood Pressure
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      30JAN2024
## Input:                     adsl.RDS, advs.RDS
## Output:                    tsfvit02.rtf
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

tblid <- "TSFVIT02"
fileid <- write_path(opath, tblid)
titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

popfl <- "SAFFL"
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"

selparamcd <- c("SYSBP", "DIABP")

### tsfvit02a
selparamcd <- c("SYSBP")

### tsfvit02b : no code updates needed
# selparamcd <- c("DIABP")

### as in dataset, order is important for later processing
### not automated, hard coded approach for ease of reading
### ideally the datasets already contain the appropriate case, to ensure units are in proper case
sel_param <- c("Systolic Blood Pressure (mmHg)")
sel_param_case <- c("Systolic blood pressure (mmHg)")

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

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)
ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)

### note: by filter ANL06FL, this table is restricted to On-treatment values, per definition of ANL06FL
### therefor, no need to add ONTRTFL in filter
### if derivation of ANL06FL is not restricted to ONTRTFL records, adding ONTRTFL here will not give the correct answer either
### as mixing worst with other period is not giving the proper selection !!!
filtered_advs <- advs_jnj %>%
  filter(PARAMCD %in% selparamcd) %>%
  filter(ANL06FL == "Y") %>%
  select(
    STUDYID,
    USUBJID,
    PARAMCD,
    PARAM,
    AVALCAT1,
    AVALCA1N,
    AVISIT,
    ANL06FL,
    APOBLFL,
    ONTRTFL
  ) %>%
  inner_join(adsl)

filtered_advs$PARAM <- factor(
  as.character(filtered_advs$PARAM),
  levels = sel_param,
  labels = sel_param_case
)


################################################################################
# Process markedly abnormal values from spreadsheet:
################################################################################

### Markedly Abnormal spreadsheet

markedlyabnormal_file <- file.path(datapath, "markedlyabnormal.xlsx")




lbmarkedlyabnormal_defs <- readxl::read_excel(
  markedlyabnormal_file,
  sheet = "ADVS"
) %>%
  filter(PARAMCD != "Parameter Code") %>%
  arrange(PARAMCD, VARNAME, ORDER) %>%
  filter(VARNAME == "AVALCAT1") %>%
  filter(PARAMCD %in% selparamcd) %>%
  left_join(., unique(filtered_advs %>% select(PARAMCD, PARAM)))


# code_decode <- getcodelistinfo(df=filtered_advs,
#                                domain="advs",
#                                vars = c("AVALCAT1"),
#                                APT= apt,
#                                adam_meta_loc=am_in,
#                                sdtm_meta_loc=dm_in)
#
#
# code_decode <- code_decode %>%
#   filter(PARAMCD %in% selparamcd) %>%
#   left_join(.,unique(filtered_advs %>% select(PARAMCD,PARAM)))

### create a mapping table for usage in split_fun trim_levels_to_map,
### to ensure only the levels appropriate for the selected parameters are covered

param_map <- lbmarkedlyabnormal_defs %>%
  rename(AVALCAT1 = CRIT) %>%
  ### no factors are allowed in this split_fun map definition
  mutate(PARAM = as.character(PARAM)) %>%
  select(PARAM, AVALCAT1)

# if no code list was defined, not all defined categories might be present on data
# sas2rds conversion might not have added all categories as factor levels - need to do this here
if (
  class(filtered_advs$AVALCAT1) == "character" ||
    !all(unique(param_map$AVALCAT1) %in% levels(filtered_advs$AVALCAT1))
) {
  filtered_advs$AVALCAT1 <- factor(
    as.character(filtered_advs$AVALCAT1),
    levels = unique(param_map$AVALCAT1)
  )
}

# filtered_advs$PARAM <- droplevels(filtered_advs$PARAM)

filtered_advs <- var_relabel(filtered_advs, PARAM = "Blood Pressure")

################################################################################
# Define layout and build table:
################################################################################

extra_args_rr <- list(
  method = "wald",
  denom = "n_df",
  ref_path = ref_path,
  .stats = c("denom", "count_unique_fraction")
)

lyt0 <- basic_table(
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
  split_rows_by(
    "PARAM",
    label_pos = "topleft",
    child_labels = "hidden",
    split_label = "Systolic Blood Pressure (mmHg), n (%)",
    section_div = " ",
    ## ensure only the appropriate levels inside PARAM-AVALCAT1 will be included
    split_fun = trim_levels_to_map(param_map)
  )

# version without explicit denominator (as in shell)
lyt <- lyt0 %>%
  # for testing, it is sometimes convenient to explicitely show the used denominator
  analyze(
    "AVALCAT1",
    a_freq_j,
    extra_args = extra_args_rr,
    show_labels = "hidden",
    indent_mod = 0L
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
