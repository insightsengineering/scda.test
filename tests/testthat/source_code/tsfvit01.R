################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsfvit01
## R version:                 4.2.1
## Short Description:         Program to create tsfvit01: Mean Change From Baseline
##                            in Blood Pressure Over Time
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      30JAN2024
## Input:                     adsl.RDS, advs.RDS
## Output:                    tsfvit01.rtf
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

tblid <- "TSFVIT01"
fileid <- write_path(opath, tblid)
titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

popfl <- "SAFFL"
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"


# Note on ancova parameter
# when ancova = TRUE
# ancova model will be used to calculate all mean/mean change columns
# not just those from the Difference column
# model specification
summ_vars <- list(arm = trtvar, covariates = NULL)

# when ancova = FALSE, all mean/mean change columns will be from descriptive stats
# for the difference column descriptive stats will be based upon two-sample t-test
ancova <- FALSE


comp_btw_group <- TRUE

selparamcd <- c("DIABP", "SYSBP")
# to Update the timepoints to what is relevant for your study
# For the trial used for the shell program, the screening timepoint is the timepoint considered as baseline
# see further, an alternative method to identify all non-unscheduled visits based upon data
selvisit <- c("Screening", "Cycle 02", "Cycle 03", "Cycle 04")

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
  select(
    STUDYID,
    USUBJID,
    all_of(c(popfl, trtvar)),
    SEX,
    AGEGR1,
    RACE,
    ETHNIC,
    AGE
  )

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

adsl$rrisk_header <- "Difference in Mean Change (95% CI)"
adsl$rrisk_label <- paste(adsl[[trtvar]], paste("vs", ctrl_grp))

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

advs00 <- advs_jnj %>%
  select(
    USUBJID,
    AVISITN,
    AVISIT,
    PARAMCD,
    PARAM,
    AVAL,
    BASE,
    CHG,
    starts_with("ANL"),
    ABLFL,
    APOBLFL,
    VSSTAT
  ) %>%
  inner_join(adsl)

# selection of all non-unscheduled visits from data
visits <- advs00 %>%
  select(AVISIT) %>%
  filter(!grepl("UNSCHEDULED", toupper(AVISIT)))

visits$AVISIT <- droplevels(visits$AVISIT)
selvisit_data <- levels(visits$AVISIT)

### if preferred to get it from data, rather than hardcoded list of visits
# selvisit <- selvisit_data

## retrieve the precision of AVAL on the input dataset
## review outcome and make updates manually if needed
## the precision variable will be used for the parameter-based formats in layout

## decimal = 4 is a cap in this derivation: if decimal precision of variable > decimal, the result will end up as decimal
## eg if AVAL has precision of 6 for parameter x, and decimal = 4, the resulting decimal value for parameter x is 4

## note that precision is on the raw values, as we are presenting mean/ci, and extra digit will be added
## eg precision = 2 will result in mean/ci format xx.xxx (xx.xxx, xx.xxx)

vs_precision <- tidytlg:::make_precision_data(
  df = advs00,
  decimal = 3,
  precisionby = "PARAMCD",
  precisionon = "AVAL"
)

### data preparation

filtered_advs <- advs00 %>%
  filter(PARAMCD %in% selparamcd) %>%
  filter(AVISIT %in% selvisit) %>%
  ### unique record per timepoint:
  filter(ANL02FL == "Y" & (ABLFL == "Y" | APOBLFL == "Y"))

#### perform check on unique record per subject/param/timepoint
check_unique <- filtered_advs %>%
  group_by(USUBJID, PARAMCD, AVISIT) %>%
  mutate(n_recsub = n()) %>%
  filter(n_recsub > 1)

if (nrow(check_unique) > 0) {
  stop(
    "Your input dataset needs extra attention, as some subjects have more than one record per parameter/visit"
  )
  ### you will run into issues with fraction portion in count_denom_fraction, as count > denom, and fraction > 1 if you don't adjust your input dataset

  # Possible extra derivation - just to ensure program can run without issues
  ### Study team is responsible for adding this derivation onto ADaM dataset and ensure proper derivation rule for ANL02FL is implemented !!!!!!!!!!
  filtered_advsx <- advs00 %>%
    filter(PARAMCD %in% selparamcd) %>%
    filter(AVISIT %in% selvisit) %>%
    ### unique record per timepoint:
    filter(ANL02FL == "Y" & (ABLFL == "Y" | APOBLFL == "Y")) %>%
    group_by(USUBJID, PARAM, AVISIT) %>%
    mutate(n_sub = n()) %>%
    arrange(USUBJID, PARAM, AVISIT, ADT) %>%
    mutate(i = vctrs::vec_group_id(ADT)) %>%
    mutate(
      ANL02FL = case_when(
        n_sub == 1 ~ "Y",
        i == 1 ~ "Y"
      )
    ) %>%
    select(-c(i, n_sub)) %>%
    ungroup()

  filtered_advs <- filtered_advsx %>%
    filter(PARAMCD %in% selparamcd) %>%
    filter(AVISIT %in% selvisit) %>%
    ### unique record per timepoint:
    filter(ANL02FL == "Y" & (ABLFL == "Y" | APOBLFL == "Y"))

  ## now your data should contain 1 record per subject per parameter
}

### for denominator per timepoint: all records from advs on this timepoint: ignoring anl01fl/anl02fl/param
filtered_advs_timepoints <- unique(
  advs00 %>%
    filter(AVISIT %in% selvisit) %>%
    select(USUBJID, AVISITN, AVISIT)
) %>%
  inner_join(adsl)

filtered_advs$PARAM <- factor(
  as.character(filtered_advs$PARAM),
  levels = sel_param,
  labels = sel_param_case
)

params <- unique(filtered_advs %>% select(PARAMCD, PARAM))

filtered_advs_timepoints <- filtered_advs_timepoints %>%
  mutate(dummy_join = 1) %>%
  full_join(
    params %>% mutate(dummy_join = 1),
    relationship = "many-to-many"
  ) %>%
  select(-dummy_join)

### identify subjects in filtered_advs_timepoints and not in filtered_advs

extra_sub <- anti_join(filtered_advs_timepoints, filtered_advs)

### only add these extra_sub to
### this will ensure we still meet the one record per subject per timepoint
### this will ensure length(x) can be used for the denominator derivation inside summarize_aval_chg_diff function

filtered_advs <- bind_rows(filtered_advs, extra_sub) %>%
  arrange(USUBJID, PARAM, AVISITN)

filtered_advs <- filtered_advs %>%
  inner_join(vs_precision, by = "PARAMCD")

### important: previous actions lost the label of variables
### in order to be able to use obj_label(filtered_advs$PARAM) in layout, need to redefine the label
filtered_advs <- var_relabel_list(filtered_advs, var_labels(advs00, fill = T))

################################################################################
# Define layout and build table:
################################################################################

summ_vars <- list(arm = trtvar, covariates = NULL)
ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)
multivars <- c("AVAL", "AVAL", "CHG")

extra_args_3col <- list(
  format_na_str = rep("NA", 3),
  d = "decimal",
  variables = summ_vars,
  ref_path = ref_path,
  ancova = ancova,
  comp_btw_group = comp_btw_group,
  multivars = multivars
)

lyt <- basic_table(show_colcounts = FALSE, colcount_format = "N=xx") %>%
  ### first columns
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map),
    show_colcounts = FALSE
  ) %>%
  split_cols_by(trtvar, show_colcounts = TRUE, colcount_format = "N=xx") %>%
  split_rows_by(
    "PARAM",
    label_pos = "topleft",
    split_label = "Blood Pressure",
    section_div = " ",
    split_fun = drop_split_levels
  ) %>%
  ## note the child_labels = hidden for AVISIT, these labels will be taken care off by
  ## applying function summarize_aval_chg_diff further in the layout
  split_rows_by(
    "AVISIT",
    label_pos = "topleft",
    split_label = "Study Visit",
    split_fun = drop_split_levels,
    child_labels = "hidden"
  ) %>%
  ## set up a 3 column split
  split_cols_by_multivar(
    multivars,
    varlabels = c(
      "n/N (%)",
      "Mean (95% CI)",
      "Mean Change From Baseline (95% CI)"
    )
  ) %>%
  ### restart for the rrisk_header columns - note the nested = FALSE option
  ### also note the child_labels = "hidden" in both PARAM and AVISIT
  split_cols_by("rrisk_header", nested = FALSE) %>%
  split_cols_by(
    trtvar,
    split_fun = remove_split_levels(ctrl_grp),
    labels_var = "rrisk_label",
    show_colcounts = TRUE,
    colcount_format = "N=xx"
  ) %>%
  ### difference columns : just 1 column & analysis needs to be done on change
  split_cols_by_multivar(multivars[3], varlabels = c(" ")) %>%
  ### the variable passed here in analyze is not used (STUDYID), it is a dummy var passing,
  ### the function summarize_aval_chg_diff grabs the required vars from cols_by_multivar calls
  analyze(
    "STUDYID",
    afun = a_summarize_aval_chg_diff_j,
    extra_args = extra_args_3col
  )

result <- build_table(lyt, filtered_advs, alt_counts_df = adsl)

################################################################################
# Post-Processing:
# - Remove the N=xx column headers for the difference vs PBO columns
################################################################################

remove_col_count2 <- function(result, string = paste("vs", ctrl_grp)) {
  mcdf <- make_col_df(result, visible_only = FALSE)
  mcdfsel <- mcdf %>%
    filter(stringr::str_detect(toupper(label), toupper(string))) %>%
    pull(path)

  for (i in seq_along(mcdfsel)) {
    facet_colcount(result, mcdfsel[[i]]) <- NA
  }

  return(result)
}

result <- remove_col_count2(result)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, titles)

################################################################################
# Convert to tbl file and output table:
################################################################################

tt_to_tlgrtf(
  result,
  file = fileid,
  nosplitin = list(cols = c(trtvar, "rrisk_header")),
  orientation = "landscape"
)
