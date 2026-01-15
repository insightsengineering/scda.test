################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsflab01a
## R version:                 4.2.1
## Short Description:         Program to create tsflab01: Mean Change From Baseline
##                            for [Laboratory Category] Laboratory Data Over Time
##                            by [Subgroup]
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      30JAN2024
## Input:                     adsl.RDS, adlb.RDS or adlbc.RDS
## Output:                    tsflab01a.rtf
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

tblid <- "TSFLAB01a"
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


subgrpvar <- "AGEGR1"
subgrplbl <- "Age: %s years"

page_by <- TRUE # Set page_by TRUE/FALSE if you (do not) wish to start a new page after a new subgroup
indent_adj <- 0L
if (page_by) {
  indent_adj <- 1L
}

## For analysis on SI units: use adlb dataset
## For analysis on Conventional units: use adlbc dataset -- shell is in conventional units

ad_domain <- "ADLB"

# see further, an alternative method to identify all non-unscheduled visits based upon data
selvisit <- c("Screening", "Baseline", "Cycle 02", "Cycle 03", "Cycle 04")

### note : this shell covers multiple tables depending on parcat3 selections

## allowed PARCAT3 selections

# parcat3sel <- "General chemistry"
# parcat3sel <- "Kidney function"
# parcat3sel <- "Liver biochemistry"
# parcat3sel <- "Lipids"
#
# ### Hematology (HM) : has 3 subcategories that should be included on one table
# parcat3sel <- c("Complete blood count","WBC differential","Coagulation studies")

# per DPS specifications, the output identifier should include the abbreviation for the category

# 1. Present laboratory tests using separate outputs for each category as follows:
#   General chemistry (GC): Sodium, Potassium, Chloride, Bicarbonate, Urea Nitrogen, Glucose, Calcium, Magnesium, Phosphate, Protein, Albumin, Creatine Kinase, Amylase, Lipase
#   Kidney function (KF): Creatinine, GFR from Creatinine
#   Liver biochemistry (LV): Alkaline Phosphatase, Alanine Aminotransferase, Aspartate Aminotransferase, Bilirubin, Prothrombin Intl. Normalized Ratio, Gamma Glutamyl Transferase
#   Lipids (LP): Cholesterol, HDL Cholesterol, LDL Cholesterol, Triglycerides
#   Hematology (HM):  Subcategory rows should be included for Complete Blood Count, White Blood Cell Differential and for Coagulation Studies
#     Complete blood count: Leukocytes, Hemoglobin, Platelets;
#     WBC differential: Lymphocytes, Neutrophils, Eosinophils;
#     Coagulation studies: Prothrombin Time, Activated Partial Thromboplastin Time.

# The output identifier should include the abbreviation for the laboratory category (eg, TSFLAB02GC for General Chemistry)

# In current template program, only 1 version is created, without the proper abbreviation appended
# The reason for this is that TSFLAB02GC is not included in the DPS system - only the core version TSFLAB02

get_abbreviation <- function(parcat3sel) {
  parcat3sel <- toupper(parcat3sel)
  abbr <- NULL
  if (length(parcat3sel) == 1) {
    if (parcat3sel == toupper("General chemistry")) {
      abbr <- "GC"
    }
    # the following line should be removed for a true study, global jjcs standards in DPS system does not include the abbreviation
    if (parcat3sel == toupper("General chemistry")) {
      abbr <- ""
    }
    #
    if (parcat3sel == toupper("Kidney function")) {
      abbr <- "KF"
    }
    if (parcat3sel == toupper("Liver biochemistry")) {
      abbr <- "LV"
    }
    if (parcat3sel == toupper("Lipids")) abbr <- "LP"
  }
  if (length(parcat3sel) > 1) {
    if (
      all(
        parcat3sel %in%
          toupper(c(
            "Complete blood count",
            "WBC differential",
            "Coagulation studies"
          ))
      )
    ) {
      abbr <- "HM"
    }
  }

  if (is.null(abbr)) {
    message("Incorrect specification of parcat3sel")
  }

  abbr
}

get_tblid <- function(tblid, parcat3sel, method = c("after", "inbetween")) {
  abbr <- get_abbreviation(parcat3sel)

  method <- match.arg(method)
  # when inbetween, the abbreviation will be added prior to the number part of the table identifier
  # when after (default), the abbreviation will be added at the end of the table identifier

  x <- 0
  if (method == "inbetween") {
    x <- regexpr(pattern = "[0-9]", tblid)[1]
  }

  if (x > 0) {
    tblid1 <- substr(tblid, 1, x - 1)
    tblid2 <- substring(tblid, x)
    tblid_new <- paste0(tblid1, abbr, tblid2)
  } else {
    tblid_new <- paste0(tblid, abbr)
  }

  return(tblid_new)
}

## parcat3 options :
# current data: Liver biochemistry, General chemistry, Lipids, Kidney function, Complete blood count, WBC differential
# according shell: General chemistry, Kidney function, Liver biochemistry, Lipids, Hematology

## not in shell: Complete blood count, WBC differential
## not in data:  Hematology

availparcat3 <- c(
  "General chemistry",
  "Kidney function",
  "Liver biochemistry",
  "Lipids",
  "Complete blood count",
  "WBC differential",
  ""
)


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


colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)
ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)


## For analysis on SI units: use adlb dataset
adlb_complete <- adlb_jnj

# selection of all non-unscheduled visits from data
visits <- adlb_complete %>%
  select(AVISIT) %>%
  filter(!grepl("UNSCHEDULED", toupper(AVISIT)))

visits$AVISIT <- droplevels(visits$AVISIT)
selvisit_data <- levels(visits$AVISIT)

### if preferred to get it from data, rather than hardcoded list of visits
# selvisit <- selvisit_data

adlb00 <- adlb_complete %>%
  select(
    USUBJID,
    AVISITN,
    AVISIT,
    starts_with("PAR"),
    AVAL,
    BASE,
    CHG,
    PCHG,
    starts_with("ANL"),
    ABLFL,
    APOBLFL
  ) %>%
  mutate(inlbdata = "Y") %>%
  inner_join(adsl) %>%
  relocate(USUBJID, PARAMCD, AVISIT, ANL02FL, ABLFL, APOBLFL)

parcat <- unique(adlb00 %>% select(starts_with("PARCAT"), PARAMCD, PARAM))

## retrieve the precision of AVAL on the input dataset
## review outcome and make updates manually if needed
## the precision variable will be used for the parameter-based formats in layout

## decimal = 4 is a cap in this derivation: if decimal precision of variable > decimal, the result will end up as decimal
## eg if AVAL has precision of 6 for parameter x, and decimal = 4, the resulting decimal value for parameter x is 4

## note that precision is on the raw values, as we are presenting mean/ci, and extra digit will be added
## eg precision = 2 will result in mean/ci format xx.xxx (xx.xxx, xx.xxx)

lb_precision <- tidytlg:::make_precision_data(
  df = adlb00,
  decimal = 3,
  precisionby = "PARAMCD",
  precisionon = "AVAL"
)

### data preparation

filtered_adlb_00 <- adlb00 %>%
  filter(AVISIT %in% selvisit) %>%
  ### unique record per timepoint:
  filter(ANL02FL == "Y" & (ABLFL == "Y" | APOBLFL == "Y"))

#### perform check on unique record per subject/param/timepoint
check_unique <- filtered_adlb_00 %>%
  group_by(USUBJID, PARAMCD, AVISIT) %>%
  mutate(n_recsub = n()) %>%
  filter(n_recsub > 1)

#### perform check on unique record per subject/param/timepoint
check_unique <- filtered_adlb_00 %>%
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
  filtered_adlb_00x <- adlb00 %>%
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

  filtered_adlb_00 <- filtered_adlb_00x %>%
    filter(PARAMCD %in% selparamcd) %>%
    filter(AVISIT %in% selvisit) %>%
    ### unique record per timepoint:
    filter(ANL02FL == "Y" & (ABLFL == "Y" | APOBLFL == "Y"))

  ## now your data should contain 1 record per subject per parameter
}

### for denominator per timepoint: all records from adlb on this timepoint: ignoring anl01fl/anl02fl/param
filtered_adlb_timepoints <- unique(
  adlb00 %>%
    filter(AVISIT %in% selvisit) %>%
    select(USUBJID, AVISITN, AVISIT, inlbdata)
) %>%
  inner_join(adsl)


################################################################################
# Core function to produce shell for specific parcat3 selection
################################################################################

build_result_parcat3 <- function(
  df = filtered_adlb_00,
  df_timepoints = filtered_adlb_timepoints,
  df_timepoints_subgroups = adlb_timepoints_subgroups,
  df_orig = adlb00,
  PARCAT3sel = NULL,
  .adsl = adsl,
  tblid,
  save2rtf = TRUE,
  .summ_vars = summ_vars,
  .trtvar = trtvar,
  .ref_path = ref_path,
  .ctrl_grp = ctrl_grp,
  .subgrpvar = subgrpvar,
  .subgrplbl = subgrplbl,
  .page_by = page_by,
  .selvisit = selvisit
) {
  tblidx <- get_tblid(tblid, PARCAT3sel)
  titles2 <- list(
    title = "Dummy Title",
    subtitles = NULL,
    main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
  )

  .ctrl_grp <- utils::tail(.ref_path, n = 1)
  multivars <- c("AVAL", "AVAL", "CHG")

  extra_args_3col <- list(
    format_na_str = rep("NA", 3),
    d = "decimal",
    ref_path = .ref_path,
    ancova = ancova,
    comp_btw_group = comp_btw_group,
    indatavar = "inlbdata",
    multivars = multivars
  )

  ### continue with data preparation
  if (!is.null(PARCAT3sel)) {
    df <- df %>%
      filter(PARCAT3 %in% PARCAT3sel)
  }

  params <- unique(df %>% select(PARAMCD, PARAM))
  selparamcd <- params$PARAMCD
  sel_param <- params$PARAM

  df_timepoints <- df_timepoints %>%
    mutate(dummy_join = 1) %>%
    full_join(
      params %>% mutate(dummy_join = 1),
      relationship = "many-to-many"
    ) %>%
    select(-dummy_join)

  ### identify subjects in df_timepoints and not in df

  extra_sub <- anti_join(df_timepoints, df) %>%
    mutate(extra_sub = "Y")

  attr(extra_sub$extra_sub, "label") <- "Extra Subject step 1"

  ### only add these extra_sub to
  ### this will ensure we still meet the one record per subject per timepoint
  ### this will ensure length(x) can be used for the denominator derivation inside summarize_aval_chg_diff function

  df <- bind_rows(df, extra_sub) %>%
    arrange(USUBJID, PARAM, AVISITN)

  df <- df %>%
    inner_join(lb_precision, by = "PARAMCD")

  #### Only In case we want the subgroup N to come from ADSL, and not just from ADVS

  ### also add adsl subjects that have no vs data --- for subgroup counts from adsl

  adlb_timepoints_subgroups <-
    .adsl %>%
    select(USUBJID) %>%
    # define factor PARAMCD/AVISIT with one category, all levels we need
    mutate(
      PARAMCD = factor(selparamcd[1], levels = selparamcd),
      AVISIT = factor(.selvisit[1], levels = .selvisit)
    ) %>%
    # expand dataset to show all levels
    tidyr::complete(., USUBJID, PARAMCD, AVISIT)

  extra_sub2 <-
    anti_join(
      df_timepoints_subgroups,
      df %>% select(USUBJID, AVISITN, AVISIT, PARAMCD, PARAM)
    ) %>%
    left_join(
      .,
      unique(df_orig %>% select(AVISITN, AVISIT, PARAMCD, PARAM))
    ) %>%
    anti_join(., extra_sub) %>%
    inner_join(.adsl) %>%
    mutate(extra_sub2 = "Y")

  attr(extra_sub2$extra_sub2, "label") <- "Extra Subject step 2"

  ### add these extra_sub dataframe as well
  ### this will ensure we still meet the one record per subject per timepoint
  ### However, by adding also subjects without data in vs, we can no longer use length(x) for the denominator derivation inside summarize_aval_chg_diff function
  df <- bind_rows(df, extra_sub2) %>%
    arrange(USUBJID, PARAM, AVISITN)

  ### important: previous actions lost the label of variables
  ### in order to be able to use obj_label(filtered_adlb$PARAM) in layout, need to redefine the label

  ## do these 2 manually, as these are not available on advs00
  attr(df$extra_sub, "label") <- "Extra Subject step 1"
  attr(df$extra_sub2, "label") <- "Extra Subject step 2"

  df <- var_relabel_list(df, var_labels(df_orig, fill = T)) %>%
    relocate(USUBJID, PARAMCD, PARAM, AVISIT, AGEGR1, AVAL, CHG)

  df$PARAM <- factor(as.character(df$PARAM), levels = sel_param)

  ### important: previous actions lost the label of variables
  ### in order to be able to use obj_label(filtered_adlb$PARAM) in layout, need to redefine the label
  df <- var_relabel_list(df, var_labels(df_orig, fill = T))

  ################################################################################
  # Define layout and build table:
  ################################################################################

  lyt <- basic_table(show_colcounts = FALSE, colcount_format = "N=xx") %>%
    ### first columns
    split_cols_by(
      "colspan_trt",
      split_fun = trim_levels_to_map(map = colspan_trt_map)
    ) %>%
    split_cols_by(.trtvar, show_colcounts = TRUE, colcount_format = "N=xx") %>%
    split_rows_by(
      .subgrpvar,
      label_pos = "hidden",
      section_div = " ",
      split_fun = drop_split_levels,
      page_by = .page_by
    ) %>%
    ### just show number of subjects in current level of subgrpvar
    ### only show this number in the first AVAL column
    summarize_row_groups(
      var = .subgrpvar,
      cfun = a_freq_j,
      extra_args = list(
        label_fstr = .subgrplbl,
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
      split_label = "Laboratory Test",
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
      .trtvar,
      split_fun = remove_split_levels(.ctrl_grp),
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

  if (nrow(df) > 0) {
    result <- build_table(lyt, df, alt_counts_df = .adsl)

    ################################################################################
    # Post-Processing:
    # - Prune table to remove when n = 0 in all columns
    # - Remove the N=xx column headers for the difference vs PBO columns
    ################################################################################

    ### alhtough this is not really likely to occur in real data, this is a problem in the current synthetic data
    ### also here, try to remove this issue

    # rps_result <- row_paths_summary(result)

    ### below code is based upon tern pruning function has_count_in_any_col, with updates to internal function h_row_first_values for the 3 column - format we are using here

    my_has_count_in_any_col <- function(atleast, ...) {
      checkmate::assert_count(atleast)
      CombinationFunction(function(table_row) {
        row_counts <- my_h_row_counts(table_row, ...)
        ### small update compared to tern::has_count_in_any_col
        ## > vs >=
        any(row_counts > atleast)
      })
    }

    my_h_row_counts <-
      function(table_row, col_names = NULL, col_indices = NULL) {
        ## no updates compared to tern::h_row_counts, beyond using the customized my_h_row_first_values function
        counts <- my_h_row_first_values(table_row, col_names, col_indices)
        checkmate::assert_integerish(counts)
        counts
      }

    my_h_row_first_values <- function(table_row,
                                      col_names = NULL,
                                      col_indices = NULL) {
      col_indices <- tern:::check_names_indices(
        table_row,
        col_names,
        col_indices
      )
      checkmate::assert_integerish(col_indices)
      checkmate::assert_subset(col_indices, seq_len(ncol(table_row)))

      # Main values are extracted
      row_vals <- row_values(table_row)[col_indices]

      ### specific updates to current situation -- 3 column layout, I want to grab the information from the n/N column, which is in first analysis of AVAL
      specific_cols <- names(row_vals)
      specific_cols <- specific_cols[stringr::str_ends(specific_cols, "AVAL")]

      row_vals <- row_vals[specific_cols]

      # Main return
      vapply(
        row_vals,
        function(rv) {
          if (is.null(rv)) {
            NA_real_
          } else {
            rv[1L]
          }
        },
        FUN.VALUE = numeric(1)
      )
    }

    more_than_0 <- my_has_count_in_any_col(atleast = 0)

    ## seem to work ok, not clear why it goes through each row twice?
    result <- prune_table(result, keep_rows(more_than_0))

    ## Remove the N=xx column headers for the difference vs PBO columns
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
  } else {
    result <- NULL
    message(paste0(
      "Parcat3 [",
      PARCAT3sel,
      "] is not present on input dataset"
    ))
    return(result)
  }

  ################################################################################
  # Set title
  ################################################################################

  result <- set_titles(result, titles2)

  if (save2rtf) {
    ################################################################################
    # Convert to tbl file and output table
    ################################################################################
    ### add the proper abbreviation to the tblid, and add opath path
    fileid <- write_path(opath, tblidx)

    tt_to_tlgrtf(
      result,
      file = fileid,
      orientation = "landscape",
      nosplitin = list(cols = c(.trtvar, "rrisk_header"))
    )
  }

  return(result)
}


################################################################################
# Apply core function to all specified levels of parcat3 selection
#  - General Chemistry (GC)
#  - Kidney Function (KF)
#  - Liver Biochemistry (LV)
#  - Lipids (LP)
#  - Hematology (HM) :
#      Complete Blood Count
#      WBC Differential
#      Coagulation Studies
################################################################################

### note : the same core tblid (TSFLAB01a) will be used for all, inside the core function build_result_parcat3 the proper abbreviation will be added

### titles will not be retrieved for these, as the table identifiers are not in the DPS system
### study teams will have to ensure all versions that are needed are included in DPS system
# result1 <- build_result_parcat3(
#   PARCAT3sel = "Liver biochemistry",
#   tblid = tblid,
#   save2rtf = TRUE
# )
# result2 <- build_result_parcat3(
#   PARCAT3sel = "Kidney function",
#   tblid = tblid,
#   save2rtf = TRUE
# )
# result3 <- build_result_parcat3(
#   PARCAT3sel = "Lipids",
#   tblid = tblid,
#   save2rtf = TRUE
# )
#
# result4 <- build_result_parcat3(
#   PARCAT3sel = c(
#     "Complete blood count",
#     "WBC differential",
#     "Coagulation studies"
#   ),
#   tblid = tblid,
#   save2rtf = TRUE
# )

### if a certain category is not present, no rtf will be generated

result <- build_result_parcat3(PARCAT3sel = "General chemistry", tblid = tblid)
