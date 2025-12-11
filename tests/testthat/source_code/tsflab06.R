################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsflab06
## R version:                 4.2.1
## Short Description:         Program to create tsflab06:
##                            Shift in [Chemistry/Hematology] Laboratory Values From Baseline to [Time Point]
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      30JAN2024
## Input:                     adsl.RDS, adlb.RDS or adlbc.RDS
## Output:                    tsflab06.rtf
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

tblid <- "TSFLAB06"
fileid <- write_path(opath, tblid)
titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

popfl <- "SAFFL"
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"

ad_domain <- "ADLB"

## select a Single time point only
selvisit <- c("Cycle 02")


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


################################################################################
# Process Data:
################################################################################

adsl <- adsl_jnj %>%
  filter(.data[[popfl]] == "Y") %>%
  select(STUDYID, USUBJID, all_of(c(popfl, trtvar)))

adsl <- adsl %>%
  mutate(
    colspan_trt = factor(
      ifelse(.data[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
      levels = c("Active Study Agent", " ")
    )
  )
## to ensure the same order as on other outputs
trt_order <- as.character((unique(
  adsl %>% select("colspan_trt", all_of(trtvar))
) %>%
  arrange(colspan_trt, .data[[trtvar]]))[[trtvar]])
adsl[[trtvar]] <- factor(as.character(adsl[[trtvar]]), levels = trt_order)


adlb_complete <- adlb_jnj


flagvars <- c("ONTRTFL", "TRTEMFL", "LVOTFL")
adlb00 <- adlb_complete %>%
  filter(ANL02FL == "Y") %>%
  filter(!is.na(ANRIND)) %>%
  filter(!is.na(BNRIND)) %>%
  filter(AVISIT %in% selvisit) %>%
  select(
    USUBJID,
    AVISITN,
    AVISIT,
    PARAMCD,
    PARAM,
    PARAMN,
    PARCAT1,
    PARCAT3,
    ANRIND,
    BNRIND,
    ANRLO,
    ANRHI,
    starts_with("ANL"),
    all_of(flagvars),
    LBSEQ,
    AVAL,
    AVALC
  )


# ensure sorting on PARAM is alphabetical
params <- sort(unique(as.character(adlb00$PARAM)))

adlb00 <- adlb00 %>%
  mutate(PARAM = factor(as.character(PARAM), levels = params)) %>%
  # single level AVISIT
  mutate(AVISIT = factor(as.character(AVISIT)))

adlb00 <- var_relabel_list(adlb00, var_labels(adlb_complete, fill = T))

filtered_adlb <- adlb00 %>%
  filter(ANL02FL == "Y") %>%
  filter(!is.na(ANRIND)) %>%
  filter(!is.na(BNRIND)) %>%
  filter(AVISIT %in% selvisit) %>%
  inner_join(adsl)

## trick for alt_counts_df to work with col splitting
# add BNRIND to adsl, all assign to extra level N (column will be used for N counts)
adslx <- adsl %>%
  mutate(BNRIND = "N") %>%
  mutate(
    BNRIND = factor(
      BNRIND,
      levels = c("N", "LOW", "NORMAL", "HIGH"),
      labels = c("N", "Low", "Normal", "High")
    )
  )

### make factor var and add extra level to BNRIND to be used as N column
filtered_adlb <- filtered_adlb %>%
  mutate(
    BNRIND = factor(
      as.character(BNRIND),
      levels = c("N", "LOW", "NORMAL", "HIGH"),
      labels = c("N", "Low", "Normal", "High")
    )
  ) %>%
  mutate(
    ANRIND = factor(
      as.character(ANRIND),
      levels = c("LOW", "NORMAL", "HIGH"),
      labels = c("Low", "Normal", "High")
    )
  )


## add variable for column split header
filtered_adlb$BNRIND_header <- "Baseline"
adslx$BNRIND_header <- "Baseline"

filtered_adlb$BNRIND_header2 <- " " ## first column N should not appear under Baseline column span
adslx$BNRIND_header2 <- " " ## first column N should not appear under Baseline column span


################################################################################
# define a  mapping for low/normal/high, tests that do not have both directions are to be identified
################################################################################
low_high_map <- unique(
  filtered_adlb %>%
    mutate(
      xANRLO = !is.na(ANRLO),
      xANRHI = !is.na(ANRHI)
    ) %>%
    select(PARCAT3, PARAMCD, PARAM, xANRLO, xANRHI) %>%
    group_by(PARAMCD) %>%
    mutate(xANRLO = any(xANRLO), xANRHI = any(xANRHI)) %>%
    ungroup()
)

# check if there are tests that only have 1 direction
lh_1 <- nrow(
  low_high_map %>%
    filter(!(xANRLO & xANRHI))
) >
  0

if (lh_1) {
  low_high <- low_high_map %>%
    mutate(ANRIND = "LOW") %>%
    mutate(ANRIND = factor(ANRIND, levels = c("LOW", "NORMAL", "HIGH"))) %>%
    mutate(PARAMCD = droplevels(PARAMCD)) %>%
    tidyr::expand(., PARAMCD, ANRIND)

  low_high_map <- low_high_map %>%
    full_join(., low_high) %>%
    mutate(
      todel = case_when(
        ANRIND == "LOW" & !xANRLO ~ TRUE,
        ANRIND == "HIGH" & !xANRHI ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    filter(!todel) %>%
    select(-c(todel, xANRLO, xANRHI))
}

### here no such tests, so there is no need to work with a mapping table for now

################################################################################
# Define layout and build table:
################################################################################

################################################################################
# Core function to produce shell for specific parcat3 selection
################################################################################

build_result_parcat3 <- function(
  df = filtered_adlb,
  PARCAT3sel = NULL,
  .adsl = adslx,
  map = low_high_map,
  tblid,
  save2rtf = TRUE,
  .trtvar = trtvar
) {
  tblidx <- get_tblid(tblid, PARCAT3sel)
  titles2 <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

  lyt_filter <- function(PARCAT3sel = NULL, map) {
    if (!is.null(PARCAT3sel)) {
      map <- map %>%
        filter(PARCAT3 %in% PARCAT3sel)

      df_filtered <- df %>%
        filter(PARCAT3 %in% PARCAT3sel)
    } else {
      df_filtered <- df
    }

    lyt <- basic_table(show_colcounts = FALSE) %>%
      ## to ensure N column is not under the Baseline column span header
      split_cols_by("BNRIND_header2") %>%
      split_cols_by("BNRIND", split_fun = keep_split_levels("N")) %>%
      split_cols_by("BNRIND_header", nested = FALSE) %>%
      split_cols_by(
        "BNRIND",
        split_fun = make_split_fun(
          pre = list(rm_levels(excl = "N")),
          post = list(add_overall_facet("TOTAL", "Total"))
        )
      ) %>%
      #### replace split_rows and summarize by single analyze call
      ### a_freq_j only works due to
      ### special arguments can do the trick : denomf = adslx & .stats = count_unique
      ### we want counts of treatment group coming from adsl, not from input dataset, therefor, countsource = altdf
      analyze(
        vars = .trtvar,
        afun = a_freq_j,
        extra_args = list(
          restr_columns = "N",
          .stats = "count_unique",
          countsource = "altdf",
          extrablankline = TRUE
        ),
        indent_mod = -1L
      ) %>%
      ## main part of table, restart row-split so nested = FALSE
      split_rows_by(
        "PARAM",
        nested = FALSE,
        label_pos = "topleft",
        child_labels = "visible",
        split_label = "Laboratory Test",
        split_fun = drop_split_levels,
        section_div = " "
      ) %>%
      split_rows_by(
        "AVISIT",
        label_pos = "topleft",
        split_label = "Study Visit",
        section_div = " "
      ) %>%
      split_rows_by(
        .trtvar,
        label_pos = "hidden",
        split_label = "Treatment Group",
        section_div = " "
      ) %>%
      ### a_freq_j
      ### the special statistic "n_rowdf" option does the trick here of getting the proper value for the N column
      summarize_row_groups(
        .trtvar,
        cfun = a_freq_j,
        extra_args = list(
          .stats = "n_rowdf",
          restr_columns = c("N")
        )
      ) %>%
      ## add extra level TOTAL using new_levels, rather than earlier technique
      ## advantage for denominator derivation -- n_rowdf can be used, if we'd like to present fraction as well
      ## switch .stats to count_unique_denom_fraction or count_unique_fraction
      analyze(
        "ANRIND",
        afun = a_freq_j,
        extra_args = list(
          .stats = "count_unique",
          denom = "n_rowdf",
          new_levels = list(c("Total"), list(c("Low", "Normal", "High"))),
          new_levels_after = TRUE,
          .indent_mods = 1L,
          restr_columns = c(
            c("LOW", "NORMAL", "HIGH", "TOTAL")
          )
        )
      )
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

### note : the same core tblid (TSFLAB06) will be used for all, inside the core function build_result_parcat3 the proper abbreviation will be added

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
