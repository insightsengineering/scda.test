################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsfvit05
## R version:                 4.2.1
## Short Description:         Program to create tsfvit05: Subjects With
##                            On-treatment Clinically Important Vital Signs [Over Time]
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      30JAN2024
## Input:                     adsl.RDS, advs.RDS
## Output:                    tsfvit05.rtf
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

tblid <- "TSFVIT05"
fileid <- write_path(opath, tblid)
titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

popfl <- "SAFFL"
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"

selparamcd <- c("PULSE", "SYSBP", "DIABP")

selvisit <- c("Cycle 02", "Cycle 04")

### as in dataset, order is important for later processing
### not automated, hard coded approach for ease of reading
### ideally the datasets already contain the appropriate case, to ensure units are in proper case
sel_param <- c(
  "Pulse Rate (beats/min)",
  "Systolic Blood Pressure (mmHg)",
  "Diastolic Blood Pressure (mmHg)"
)
sel_param_case <- c(
  "Pulse rate (beats/min)",
  "Systolic blood pressure (mmHg)",
  "Diastolic blood pressure (mmHg)"
)

## different variants of table
over_time <- TRUE # over time analysis, ALSO restricted to on-treatment records
worst <- TRUE # analysis of min/max value, restricted to on-treatment records
## you can set both worst and over_time to TRUE, the "worst" timepoint will be shown first
treatment_emergent <- FALSE # only consider te values

relrisk <- FALSE
# by default JJCS there is no need to add relative risk columns

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

### for analysis of treatment emergent flag, do not use TRTEMFL as filter, as this will not give the proper denominator

if (over_time) {
  ## Version 1 : over time
  filtered_advs_1 <- advs_jnj %>%
    filter(PARAMCD %in% selparamcd) %>%
    filter(AVISIT %in% selvisit) %>%
    filter(ANL02FL == "Y") %>%
    ### note regarding ONTRTFL --- Per agreement on June 17 : On treatment over time : Filter on ONTRTFL as well
    ###
    filter(ONTRTFL == "Y") %>%
    select(
      STUDYID,
      USUBJID,
      PARAMCD,
      PARAM,
      AVALCAT1,
      AVALCA1N,
      AVISIT,
      ONTRTFL,
      APOBLFL,
      CRIT1,
      CRIT1FL,
      CRIT2,
      CRIT2FL,
      TRTEMFL,
      ANL02FL,
      AVAL
    ) %>%
    inner_join(adsl)
}

if (worst) {
  ## Version 2 : "worst analysis" -- ANL03FL (max) ANL06FL (min)

  ### note: by filter ANL06FL/ANL03FL, this table is restricted to On-treatment values, per definition of ANL06FL/ANL03FL
  ### therefor, no need to add ONTRTFL in filter
  ### if derivation of ANL06FL is not restricted to ONTRTFL records, adding ONTRTFL here will not give the correct answer either
  ### as mixing worst with other period is not giving the proper selection !!!

  filtered_advs_2 <- advs_jnj %>%
    filter(PARAMCD %in% selparamcd) %>%
    # 2 records per subject selected : min (ANL06) / max (ANL03)
    filter(ANL03FL == "Y" | ANL06FL == "Y") %>%
    select(
      STUDYID,
      USUBJID,
      PARAMCD,
      PARAM,
      AVALCAT1,
      AVALCA1N,
      AVISIT,
      APOBLFL,
      CRIT1,
      CRIT1FL,
      CRIT2,
      CRIT2FL,
      TRTEMFL,
      ANL03FL,
      ANL06FL,
      AVAL
    ) %>%
    inner_join(adsl)

  ### need to check for your study if the below applies as these can be study specific
  ## for CRIT1 : use minimum (ANL06FL) - disregard the record if it was not coming from MIN
  ## for CRIT2 : use maximum (ANL03FL) - disregard the record if it was not coming from MAX
  ## 1 record per subjects with non-missing CRIT1FL
  ## 1 record per subjects with non-missing CRIT2FL
  ## these will be over 2 different records
  filtered_advs_2 <- filtered_advs_2 %>%
    mutate(
      CRIT1FL = case_when(
        ANL06FL == "N" ~ NA,
        TRUE ~ CRIT1FL
      ),
      CRIT2FL = case_when(
        ANL03FL == "N" ~ NA,
        TRUE ~ CRIT2FL
      )
    ) %>%
    mutate(AVISIT = "On-treatment")
}


if (over_time & worst) {
  filtered_advs <- bind_rows(
    filtered_advs_1,
    filtered_advs_2
  )

  filtered_advs$AVISIT <- factor(
    as.character(filtered_advs$AVISIT),
    levels = c("On-treatment", levels(filtered_advs_1$AVISIT))
  )
} else if (over_time) {
  filtered_advs <- filtered_advs_1
} else if (worst) {
  filtered_advs <- filtered_advs_2
} else {
  stop("At least worst or over_time should be set to TRUE")
}

filtered_advs$PARAM <- factor(
  as.character(filtered_advs$PARAM),
  levels = sel_param,
  labels = sel_param_case
)

### for analysis of treatment emergent abnormalities only :
### per discussion with DAS team: denominator is still the subjects with evaluation of CRIT1FL/CRIT2FL,
### there is no need to get baseline status to exclude subjects with an abnormal baseline status
### the only thing that should be done is : non-treatment emergent abnormalities should not be reported in the numerator, only in denom
### this can be done using the following code

if (treatment_emergent) {
  filtered_advs <- filtered_advs %>%
    mutate(
      CRIT1FL = case_when(
        CRIT1FL == "Y" & (is.na(TRTEMFL) | TRTEMFL != "Y") ~ "N",
        TRUE ~ CRIT1FL
      ),
      CRIT2FL = case_when(
        CRIT2FL == "Y" & (is.na(TRTEMFL) | TRTEMFL != "Y") ~ "N",
        TRUE ~ CRIT2FL
      )
    )
}


filtered_advs <- var_relabel(filtered_advs, PARAM = "Blood Pressure (mmHg)")


### Mapping for CRIT1&CRIT2
### alternative approach to retrieve from metadata iso dataset -
xlabel_map <- unique(filtered_advs %>% select(PARAM, CRIT1, CRIT2)) %>%
  tidyr::pivot_longer(
    cols = c("CRIT1", "CRIT2"),
    names_to = "var",
    values_to = "label"
  ) %>%
  filter(!is.na(label)) %>%
  mutate(
    label = as.character(label),
    var = paste0(var, "FL"),
    value = "Y"
  )

################################################################################
# Define layout and build table:
################################################################################

extra_args_rr <- list(
  method = "wald",
  denom = "n_df",
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
  split_cols_by(trtvar)


if (relrisk) {
  lyt <- lyt %>%
    split_cols_by("rrisk_header", nested = FALSE) %>%
    split_cols_by(
      trtvar,
      labels_var = "rrisk_label",
      split_fun = remove_split_levels(ctrl_grp)
    )
}

if (over_time) {
  lyt <- lyt %>%
    split_rows_by(
      "AVISIT",
      label_pos = "topleft",
      child_labels = "visible",
      split_label = "Study Visit",
      split_fun = drop_split_levels,
      section_div = " "
    )
}

lyt <- lyt %>%
  split_rows_by(
    "PARAM",
    label_pos = "topleft",
    child_labels = "visible",
    split_label = "Parameter",
    split_fun = drop_split_levels,
    section_div = " "
  ) %>%
  analyze(
    "CRIT1FL",
    a_freq_j,
    show_labels = "hidden",
    table_names = "CRIT1_N",
    extra_args = list(denom = "n_df", .stats = c("n_df"))
  ) %>%
  analyze(
    c("CRIT1FL", "CRIT2FL"),
    a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(val = c("Y"), label_map = xlabel_map)
    ),
    show_labels = "hidden",
    indent_mod = 1L
  ) %>%
  append_topleft("   Criteria, n (%)")


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

tt_to_tlgrtf(result, file = fileid)
