################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsfecg01
## R version:                 4.2.1
## Short Description:         Program to create tsfecg01: Mean Change From Baseline
##                            for ECG Data Over Time by [Subgroup]
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      30JAN2024
## Input:                     adsl.RDS, adeg.RDS
## Output:                    tsfecg01.rtf
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

tblid <- "TSFECG01a"
fileid <- write_path(opath, tblid)
titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

popfl <- "SAFFL"
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"
subgrpvar <- "AGEGR1"
subgrplbl <- "Age: %s years"

page_by <- FALSE # Set page_by TRUE/FALSE if you (do not) wish to start a new page after a new subgroup
indent_adj <- -1L
if (page_by) {
  indent_adj <- 0L
}

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


selparamcd <- c("EGHRMN", "PRAG", "QRSAG", "QTC", "QTCBAG", "QTCFAG", "RRAG")
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

# For template output to reduce output, only limited timepoints and parameters are selected
# Remove for actual study code
selparamcd <- c("EGHRMN", "PRAG")
# see further, an alternative method to identify all non-unscheduled visits based upon data
selvisit <- c("Baseline", "Month 1", "Month 3")


################################################################################
# Process Data:
################################################################################

adsl <- adsl_jnj %>%
  filter(.data[[popfl]] == "Y") %>%
  select(
    STUDYID,
    USUBJID,
    all_of(c(popfl, trtvar, subgrpvar)),
    SEX,
    AGEGR1,
    RACE,
    ETHNIC,
    AGE
  )


msubgrp <- adsl %>%
  group_by(across(all_of(c(trtvar, subgrpvar)))) %>%
  summarize(count = n())

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

adsl$rrisk_header <- "Difference in Mean Change (95% CI)"
adsl$rrisk_label <- paste(adsl[[trtvar]], paste("vs", ctrl_grp))


adeg00 <- adeg_jnj %>%
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
    APOBLFL
    # ,EGSTAT
  ) %>%
  mutate(inegdata = "Y") %>%
  inner_join(adsl)

# selection of all non-unscheduled visits from data
visits <- adeg00 %>%
  select(AVISIT) %>%
  filter(!grepl("UNSCHEDULED", toupper(AVISIT)))

visits$AVISIT <- droplevels(visits$AVISIT)
selvisit_data <- levels(visits$AVISIT)

### if preferred to get it from data, rather than hardcoded list of visits
# selvisit <- selvisit_data

## retrieve the precision of AVAL on the input dataset
## review outcome and make updates manually if needed
## the precision variable will be used for the parameter-based formats in layout

## decimal = 3 is a cap in this derivation: if decimal precision of variable > decimal, the result will end up as decimal
## eg if AVAL has precision of 6 for parameter x, and decimal = 3, the resulting decimal value for parameter x is 3

## note that precision is on the raw values, as we are presenting mean/ci, and extra digit will be added
## eg precision = 2 will result in mean/ci format xx.xxx (xx.xxx, xx.xxx)

eg_precision <- tidytlg:::make_precision_data(
  df = adeg00,
  decimal = 3,
  precisionby = "PARAMCD",
  precisionon = "AVAL"
)

filtered_adeg_00 <- adeg00 %>%
  filter(PARAMCD %in% selparamcd) %>%
  filter(AVISIT %in% selvisit) %>%
  ### unique record per timepoint:
  filter(ANL02FL == "Y" & (ABLFL == "Y" | APOBLFL == "Y"))

#### perform check on unique record per subject/param/timepoint
check_unique <- filtered_adeg_00 %>%
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
  filtered_adeg_00x <- adeg00 %>%
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

  filtered_adeg_00 <- filtered_adeg_00x %>%
    filter(PARAMCD %in% selparamcd) %>%
    filter(AVISIT %in% selvisit) %>%
    ### unique record per timepoint:
    filter(ANL02FL == "Y" & (ABLFL == "Y" | APOBLFL == "Y"))

  ## now your data should contain 1 record per subject per parameter
}


### for denominator per timepoint: all records from adeg on this timepoint: ignoring anl01fl/anl02fl/param
filtered_adeg_timepoints <- unique(
  adeg00 %>%
    filter(AVISIT %in% selvisit) %>%
    select(USUBJID, AVISITN, AVISIT, inegdata)
) %>%
  inner_join(adsl)

params <- unique(filtered_adeg_00 %>% select(PARAMCD, PARAM))

filtered_adeg_timepoints <- filtered_adeg_timepoints %>%
  mutate(dummy_join = 1) %>%
  full_join(
    params %>% mutate(dummy_join = 1),
    relationship = "many-to-many"
  ) %>%
  select(-dummy_join)

### identify subjects in filtered_adeg_timepoints and not in filtered_adeg

extra_sub <- anti_join(filtered_adeg_timepoints, filtered_adeg_00) %>%
  mutate(extra_sub = "Y")

attr(extra_sub$extra_sub, "label") <- "Extra Subject step 1"

### add these extra_sub dataframes to
### this will ensure we still meet the one record per subject per timepoint
### note that we can no longer use length(x) can be used for the denominator derivation inside summarize_aval_chg_diff function
filtered_adeg <- bind_rows(filtered_adeg_00, extra_sub) %>%
  arrange(USUBJID, PARAM, AVISITN)

#### Only In case we want the subgroup N to come from ADSL, and not just from adeg

### also add adsl subjects that have no vs data --- for subgroup counts from adsl

adeg_timepoints_subgroups <-
  adsl %>%
  select(USUBJID) %>%
  # define factor PARAMCD/AVISIT with one category, all levels we need
  mutate(
    PARAMCD = factor(selparamcd[1], levels = selparamcd),
    AVISIT = factor(selvisit[1], levels = selvisit)
  ) %>%
  # expand dataset to show all levels
  tidyr::complete(., USUBJID, PARAMCD, AVISIT)


extra_sub2 <-
  anti_join(
    adeg_timepoints_subgroups,
    filtered_adeg_00 %>% select(USUBJID, AVISITN, AVISIT, PARAMCD, PARAM)
  ) %>%
  left_join(., unique(adeg00 %>% select(AVISITN, AVISIT, PARAMCD, PARAM))) %>%
  anti_join(., extra_sub) %>%
  inner_join(adsl) %>%
  mutate(extra_sub2 = "Y")

attr(extra_sub2$extra_sub2, "label") <- "Extra Subject step 2"

### add these extra_sub dataframe as well
### this will ensure we still meet the one record per subject per timepoint
### However, by adding also subjects without data in vs, we can no longer use length(x) for the denominator derivation inside summarize_aval_chg_diff function
filtered_adeg <- bind_rows(filtered_adeg, extra_sub2) %>%
  arrange(USUBJID, PARAM, AVISITN)


filtered_adeg <- filtered_adeg %>%
  inner_join(eg_precision, by = "PARAMCD")

### important: previous actions lost the label of variables
### in order to be able to use obj_label(filtered_adeg$PARAM) in layout, need to redefine the label

## do these 2 manually, as these are not available on adeg00
attr(filtered_adeg$extra_sub, "label") <- "Extra Subject step 1"
attr(filtered_adeg$extra_sub2, "label") <- "Extra Subject step 2"

filtered_adeg <- var_relabel_list(filtered_adeg, var_labels(adeg00, fill = T))


#### Note : variable inegdata will be used to differentiate records from subjects in vsdata at timepoint and subjects not in vsdata (only in adsl)
#### is.na(filtered_adeg$inegdata) corresponds to filtered_adeg$extra_sub2 == "Y"

## check1 <- filtered_adeg %>% filter(extra_sub2 == "Y") %>% select(USUBJID,inegdata,extra_sub2,PARAMCD,AVISIT,AVAL)
## check2 <- filtered_adeg %>% filter(is.na(inegdata)) %>% select(USUBJID,inegdata,extra_sub2,PARAMCD,AVISIT,AVAL)

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
  indatavar = "inegdata",
  multivars = multivars
)

lyt_0 <- basic_table(show_colcounts = FALSE, colcount_format = "N=xx") %>%
  ### first columns
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) %>%
  split_cols_by(trtvar, show_colcounts = TRUE, colcount_format = "N=xx") %>%
  split_rows_by(
    subgrpvar,
    label_pos = "hidden",
    section_div = " ",
    split_fun = drop_split_levels,
    page_by = page_by
  ) %>%
  ### just show number of subjects in current level of subgrpvar
  ### only show this number in the first AVAL column
  summarize_row_groups(
    var = subgrpvar,
    cfun = a_freq_j,
    extra_args = list(
      label_fstr = subgrplbl,
      extrablankline = TRUE,
      restr_columns = "AVAL",
      .stats = c("n_altdf"),
      riskdiff = FALSE,
      denom_by = subgrpvar
    )
  ) %>%
  split_rows_by(
    "PARAM",
    label_pos = "topleft",
    split_label = obj_label(filtered_adeg$PARAM),
    section_div = " ",
    split_fun = drop_split_levels,
    indent_mod = indent_adj
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
  split_cols_by_multivar(multivars[3], varlabels = c(" "))


## layout as specified in shell
lyt <- lyt_0 %>%
  ### the variable passed here in analyze is not used (STUDYID), it is a dummy var passing,
  ### the function summarize_aval_chg_diff grabs the required vars from cols_by_multivar calls
  analyze(
    "STUDYID",
    afun = a_summarize_aval_chg_diff_j,
    extra_args = extra_args_3col
  )

result <- build_table(lyt, filtered_adeg, alt_counts_df = adsl)


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
# Convert to tbl file and output table
################################################################################

tt_to_tlgrtf(
  result,
  file = fileid,
  nosplitin = list(cols = c(trtvar, "rrisk_header")),
  orientation = "landscape"
)
