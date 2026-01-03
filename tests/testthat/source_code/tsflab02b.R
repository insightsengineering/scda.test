################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsflab02b
## R version:                 4.2.1
## Short Description:         Program to create tsflab02b: Subjects With
##                            [Laboratory Category] Laboratory Abnormalities Over Time
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      30JAN2024
## Input:                     adsl.RDS, adlb.RDS or adlbc.RDS
## Output:                    tsflab02b.rtf
##                            Multiple tables are generated using this script
##                            These tables should be defined in Autocode (or titles file) as separate tables
##                            -tsflab02bgc General chemistry
##                            -tsflab02bkf Kidney function
##                            -tsflab02blv Liver biochemistry
##                            -tsflab02blp Lipids
##                            -tsflab02bhm Hematology
##
## Remarks:                   Should only be used when abnormalities are based upon markedly abnormal file
##                              carefully review factor levels of variables MCRIT1/MCRIT2 and MCRIT1ML/MCRIT2ML on your input adlb.rds dataset
##                              Pay special attention to the following tests:
##                                    Especially check if N is correct (review ADaM derivation for these params)
##                                    GLUC : Glucose, high : LBFAST is part of condition
##                                    HDL  : separate levels for male/female
##                                    HGB  : separate levels for male/female
##                              Pay special attention to the following tests:
##
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

tblid <- "TSFLAB02b"
fileid <- write_path(opath, tblid)
titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

popfl <- "SAFFL"
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"

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


### the varying fileid will be handled at the end of the program, as this program will generate all levels

ad_domain <- "adlb"


selvisit <- c("Screening", "Cycle 02", "Cycle 03", "Cycle 04")
## if the option TRTEMFL needs to be added to the TLF -- ensure the same setting as in tsflab04
trtemfl <- TRUE

################################################################################
# Initial processing of data + check if table is valid for trial:
################################################################################
adlb_complete <- adlb_jnj


################################################################################
# Process markedly abnormal values from spreadsheet:
################################################################################

### Markedly Abnormal spreadsheet

markedlyabnormal_file <- file.path(datapath, "markedlyabnormal.xlsx")



lbmarkedlyabnormal_defs <- readxl::read_excel(
  markedlyabnormal_file,
  sheet = toupper(ad_domain)
) %>%
  filter(PARAMCD != "Parameter Code")

MCRITs <- unique(
  lbmarkedlyabnormal_defs %>%
    filter(!stringr::str_ends(VARNAME, "ML")) %>%
    pull(VARNAME)
)

MCRITs_def <- unique(
  lbmarkedlyabnormal_defs %>%
    filter(VARNAME %in% MCRITs) %>%
    select(PARAMCD, VARNAME, CRIT, SEX)
) %>%
  mutate(VARNAME = paste0(VARNAME, "ML")) %>%
  rename(CRITNAME = CRIT) %>%
  mutate(
    CRITDIR = case_when(
      VARNAME == "MCRIT1ML" ~ "DIR1",
      VARNAME == "MCRIT2ML" ~ "DIR2"
    )
  )


MCRITs_def2 <- lbmarkedlyabnormal_defs %>%
  filter(VARNAME %in% paste0(MCRITs, "ML")) %>%
  mutate(CRITn = as.character(4 - as.numeric(ORDER)))

MCRITs_def3 <- MCRITs_def2 %>%
  left_join(., MCRITs_def, relationship = "many-to-one") %>%
  select(PARAMCD, CRITNAME, CRITDIR, SEX, VARNAME, CRIT, CRITn) %>%
  arrange(PARAMCD, VARNAME, CRITDIR, SEX, CRITn) %>%
  select(-SEX)

### convert dataframe into label_map that can be used with the a_freq_j afun function
xlabel_map <- MCRITs_def3 %>%
  rename(var = VARNAME, label = CRIT) %>%
  select(PARAMCD, CRITNAME, CRITDIR, var, label)

xlabel_map2 <- xlabel_map %>%
  mutate(
    MCRIT12 = CRITNAME,
    MCRIT12ML = label
  )

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

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)
ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)

obs_mcrit12 <- unique(c(
  unique(adlb_complete$MCRIT1),
  unique(adlb_complete$MCRIT2)
))

adlb00 <- adlb_complete %>%
  filter(PARCAT2 == "Test with FDA abnormality criteria defined") %>%
  select(
    USUBJID,
    PARCAT1,
    PARCAT2,
    PARCAT3,
    ONTRTFL,
    TRTEMFL,
    PARAM,
    PARAMCD,
    AVISITN,
    AVISIT,
    AVAL,
    MCRIT1,
    MCRIT1ML,
    MCRIT2,
    MCRIT2ML,
    ONTRTFL,
    TRTEMFL,
    LVOTFL,
    ABLFL,
    ANL01FL,
    ANL02FL,
    ANL04FL,
    ANL05FL
    ### if per period/phase is needed, use below flag variables
    # ,ANL07FL,ANL08FL,ANL09FL,ANL10FL
  ) %>%
  inner_join(adsl)


combodf <- tribble(
  ~valname, ~label, ~levelcombo, ~exargs,
  "xan-comb", "Xanomeline Combined", c("Xanomeline High Dose", "Xanomeline Low Dose"), list()
)

################################################################################
##### vertical approach for analyzing MCRIT1/MCRIT2:
# filtering is easier, as well as the analyze/layout setup
################################################################################

adlb_mcrit1 <- adlb00 %>%
  filter(!is.na(MCRIT1)) %>%
  mutate(
    MCRIT12 = MCRIT1,
    MCRIT12ML = MCRIT1ML,
    CRITDIR = "DIR1"
  )

adlb_mcrit2 <- adlb00 %>%
  filter(!is.na(MCRIT2)) %>%
  mutate(
    MCRIT12 = MCRIT2,
    MCRIT12ML = MCRIT2ML,
    CRITDIR = "DIR2"
  )

adlb_mcrit <- rbind(adlb_mcrit1, adlb_mcrit2) %>%
  mutate(CRITDIR = factor(CRITDIR)) %>%
  filter(AVISIT %in% selvisit) %>%
  ### unique record per timepoint:
  filter(ANL02FL == "Y") %>%
  # ### Restrict to On-treatment / not clear???
  # To be in line with Other over time tables - we do NOT filter on ONTRTFL - awaiting confirmation
  # filter(ONTRTFL == "Y" | ABLFL == "Y") %>%

  inner_join(., adsl)

#### DO NOT USE TRTEMFL = Y in filter, as this will remove subjects from both numerator and denominator
#### instead : set MCRIT12ML to a non-reportable value (ie Level 0) and keep in dataset
if (trtemfl) {
  origlevs <- levels(adlb_mcrit$MCRIT12ML)
  adlb_mcrit <- adlb_mcrit %>%
    mutate(
      MCRIT12ML = case_when(
        !is.na(MCRIT12ML) & is.na(TRTEMFL) | TRTEMFL != "Y" ~ "Level 0",
        TRUE ~ MCRIT12ML
      )
    ) %>%
    mutate(MCRIT12ML = factor(MCRIT12ML, levels = origlevs))
}

################################################################################
##### finalize mapping dataframe based upon abnormal spreadsheet
################################################################################

xlabel_map3 <- xlabel_map2 %>%
  right_join(., unique(adlb_mcrit %>% select(PARAMCD, PARCAT3))) %>%
  arrange(PARCAT3, PARAMCD, CRITDIR, MCRIT12, MCRIT12ML) %>%
  mutate_if(is.factor, as.character) %>%
  #### get rid of mapping defined in spreadsheet but not present in data
  filter(MCRIT12 %in% obs_mcrit12)

### this will ensure alphabetical sorting on abnormality
### within a test LOW needs to come prior to High
### for this reason, split a test like 'Calcium, low' and 'Calcium, High' in 2
xlabel_map3 <- xlabel_map3 %>%
  mutate(MCRIT12x = stringr::str_split_i(MCRIT12, ",", 1)) %>%
  arrange(MCRIT12x, CRITDIR, MCRIT12ML)

# MCRIT12ML needs to be a factor, with all levels (also unobserved),
# as these levels are not available on the metadata files, only in markedly abnormal
# we need to update the factor levels
# these are present in the markedly abnormal file processing, ie we can use xlabel_map3
adlb_mcrit$MCRIT12ML <- factor(
  as.character(adlb_mcrit$MCRIT12ML),
  levels = unique(xlabel_map3$MCRIT12ML)
)


################################################################################
# Define layout and build table:
################################################################################

.extra_args_rr <- list(
  method = "wald",
  denom = "n_df",
  ref_path = ref_path,
  .stats = c("count_unique_fraction")
)

################################################################################
# Core function to produce shell for specific parcat3 selection
################################################################################

build_result_parcat3 <- function(
  df = adlb_mcrit,
  PARCAT3sel = NULL,
  .adsl = adsl,
  map = xlabel_map3,
  tblid,
  save2rtf = TRUE,
  extra_args_rr = .extra_args_rr,
  .trtvar = trtvar,
  .ref_path = ref_path,
  .ctrl_grp = ctrl_grp
) {
  ### !!!! Map dataframe should not contain more tests than in data
  ### as we need to split by PARCAT3, need to have a function for lty with the appropriate PARCAT3 selection
  ### filter of the data, original factor levels can remain, no need to drop these levels

  tblidx <- get_tblid(tblid, PARCAT3sel)
  titles2 <- list(
    title = "Dummy Title",
    subtitles = NULL,
    main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
  )

  lyt_filter <- function(PARCAT3sel = NULL, map) {
    if (!is.null(PARCAT3sel)) {
      map <- map %>%
        filter(PARCAT3 %in% PARCAT3sel)
    }

    lyt <- basic_table(show_colcounts = TRUE, colcount_format = "N=xx") %>%
      split_cols_by(
        "colspan_trt",
        split_fun = trim_levels_to_map(map = colspan_trt_map)
      ) %>%
      split_cols_by(
        .trtvar
        # , split_fun = add_combo_levels(combodf)
      ) %>%
      split_cols_by("rrisk_header", nested = FALSE) %>%
      split_cols_by(
        .trtvar,
        labels_var = "rrisk_label",
        split_fun = remove_split_levels(.ctrl_grp)
      )
    ### add in by avisit processing
    lyt <- lyt %>%
      split_rows_by(
        "AVISIT",
        label_pos = "topleft",
        section_div = " ",
        split_fun = drop_split_levels,
        split_label = "Time Point"
      ) %>%
      summarize_row_groups(
        var = "AVISIT",
        cfun = a_freq_j,
        extra_args = list(
          .stats = "n_df",
          riskdiff = FALSE
        ),
        indent_mod = -1L
      )

    lyt <- lyt %>%
      split_rows_by(
        "PARAMCD",
        split_label = "Laboratory Test",
        label_pos = "topleft",
        indent_mod = 1L,
        child_labels = "hidden",
        split_fun = trim_levels_to_map(map)
      ) %>%
      ## Low prior to High
      split_rows_by(
        "CRITDIR",
        label_pos = "hidden",
        child_labels = "hidden",
        split_fun = trim_levels_to_map(map)
      ) %>%
      split_rows_by(
        "MCRIT12",
        split_label = "Threshold Level, n (%)",
        label_pos = "topleft",
        split_fun = trim_levels_to_map(map),
        child_labels = "visible",
        section_div = " "
      ) %>%
      ### to mimic layout if analyze would be used instead
      ### child_labels has been set to visible in previous step
      summarize_row_groups(
        "MCRIT12",
        cfun = a_freq_j,
        extra_args = list(
          .stats = "n_df",
          label = "N",
          riskdiff = FALSE
        )
      ) %>%
      # denominators are varying per test, no need to show as N is shown in line above
      analyze(
        c("MCRIT12ML"),
        a_freq_j,
        extra_args = extra_args_rr,
        show_labels = "hidden",
        indent_mod = 1L
      )

    return(lyt)
  }

  lyt <- lyt_filter(PARCAT3sel = PARCAT3sel, map = map)

  if (!is.null(PARCAT3sel)) {
    df <- df %>%
      filter(PARCAT3 %in% PARCAT3sel)
  }

  if (nrow(df) > 0) {
    result <- build_table(lyt, df, alt_counts_df = .adsl)
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
  # Remove Level 0 line
  ################################################################################

  remove_grade0 <- function(tr) {
    if (is(tr, "DataRow") & (tr@label == "Level 0")) {
      return(FALSE)
    } else if (is(tr, "DataRow") & (tr@label == no_data_to_report_str)) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }

  result <- result %>% prune_table(prune_func = keep_rows(remove_grade0))

  ################################################################################
  # Remove unwanted column counts
  ################################################################################

  result <- remove_col_count(result)

  ################################################################################
  # Prune table to remove when n = 0 in all columns
  ################################################################################
  ### alhtough this is not really likely to occur in real data, this is a problem in the current synthetic data
  ### also here, try to remove this issue

  ### below code is based upon
  ### 1. tern pruning function all_zero_or_na
  ### 2. tern CombinationFunction
  ### 3a. tern pruning function h_content_first_row
  ### 3b. tern pruning function keep_content_rows

  ### 1. similar to tern pruning function all_zero_or_na
  my_all_zero_or_na <- function(tr) {
    # Main values are extracted
    row_vals <- row_values(tr)

    if (!is(tr, "TableRow") || is(tr, "LabelRow")) {
      return(FALSE)
    }
    rvs <- unlist(unname(row_vals))

    ### last condition is different versus all_zero_or_na
    all(is.na(rvs) | rvs == 0 | rvs == "")
  }

  ### 2. tern CombinationFunction
  is_non_empty <- !CombinationFunction(my_all_zero_or_na)

  ### 3a. similar to tern pruning function h_content_first_row
  h_content_second_row <- function(table) {
    ct <- content_table(table)
    tree_children(ct)[[2]]
  }

  ### 3b. based uupon tern pruning function keep_content_rows
  my_keep_content_rows <- function(content_row_condition) {
    checkmate::assert_function(content_row_condition)
    function(table_tree) {
      if (tern:::is_leaf_table(table_tree)) {
        ### take second row from the content rather than first
        content_row <- h_content_first_row(table_tree)
        return(!content_row_condition(content_row))
      }
      if (inherits(table_tree, "DataRow")) {
        return(FALSE)
      }
      children <- tree_children(table_tree)
      identical(length(children), 0L)
    }
  }

  result <- prune_table(result, my_keep_content_rows(is_non_empty))

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

    tt_to_tlgrtf(result, file = fileid, orientation = "landscape")
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

### note : the same core tblid (TSFLAB02) will be used for all, inside the core function build_result_parcat3 the proper abbreviation will be added

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

result <- build_result_parcat3(
  PARCAT3sel = "General chemistry",
  tblid = tblid,
  save2rtf = TRUE
)
