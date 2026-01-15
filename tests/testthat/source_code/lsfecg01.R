###############################################################################
## Original Reporting Effort: Standards
## Program Name:              lsfecg01.R
## R version:                 4.2.1
## Short Description:         Create LSFECG01: Listing of Subjects With
##                            [Criteria] ECG Values
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      2024-01-24
## Input:                     ADEG
## Output:                    lsfecg01.rtf
## Remarks:
## R-functions:
## R-function Sample Call:
##
## Modification History:
##  Rev #:
##  Modified By:
##  Reporting Effort:
##  Date:
##  Description:
###############################################################################

###############################################################################
# Prep environment
###############################################################################

library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(rlistings)
library(junco)
library(tidytlg)

###############################################################################
# Define script level parameters
###############################################################################

tblid <- "LSFECG01"
fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01A"
key_cols <- c("COL0", "COL1", "COL2", "COL3")
disp_cols <- paste0("COL", 0:9)
concat_sep <- " / "
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)


###############################################################################
# Process data
###############################################################################

adeg <- adeg_jnj %>%
  filter(!!rlang::sym(popfl) == "Y")

adeg_crit <- adeg %>%
  # Update CRITyFL variabes based on study requirements
  # filter(CRIT1FL == "Y" | CRIT2FL == "Y") %>%
  filter(CRIT2FL == "Y") %>%
  select(STUDYID, USUBJID, PARAMCD) %>%
  distinct()

adeg_dig <- tidytlg:::make_precision_data(
  df = adeg,
  decimal = 4,
  precisionby = "PARAMCD",
  precisionon = "AVAL"
) %>%
  rename(c(VALDIGMAX = "decimal"))

adeg_list <- adeg_crit %>%
  inner_join(
    adeg,
    by = c(
      "STUDYID" = "STUDYID",
      "USUBJID" = "USUBJID",
      "PARAMCD" = "PARAMCD"
    )
  ) %>%
  inner_join(adeg_dig, by = c("PARAMCD" = "PARAMCD"))

lsting <- adeg_list %>%
  mutate(
    AGE = explicit_na(as.character(AGE), ""),
    SEX = explicit_na(SEX, ""),
    RACE_DECODE = explicit_na(RACE_DECODE, ""),
    ADT = ifelse(
      nchar(as.character(ADT)) == 10,
      toupper(format(ADT, "%d%b%Y")),
      "---------"
    ),
    ATM = ifelse(!is.na(ADTM), substr(as.character(ADTM), 12, 16), "--:--"),
    ADYN = ifelse(!is.na(ADY), ADY, NA),
    ADY = ifelse(!is.na(ADY), ADY, "--"),
    VAL_RES = case_when(
      is.na(VALDIGMAX) & !is.na(AVAL) ~
        tidytlg::roundSAS(AVAL, digits = 0, as_char = TRUE, na_char = NULL),
      VALDIGMAX == 0 & !is.na(AVAL) ~
        tidytlg::roundSAS(AVAL, digits = 0, as_char = TRUE, na_char = NULL),
      VALDIGMAX == 1 & !is.na(AVAL) ~
        tidytlg::roundSAS(AVAL, digits = 1, as_char = TRUE, na_char = NULL),
      VALDIGMAX == 2 & !is.na(AVAL) ~
        tidytlg::roundSAS(AVAL, digits = 2, as_char = TRUE, na_char = NULL),
      VALDIGMAX == 3 & !is.na(AVAL) ~
        tidytlg::roundSAS(AVAL, digits = 3, as_char = TRUE, na_char = NULL),
      VALDIGMAX >= 4 & !is.na(AVAL) ~
        tidytlg::roundSAS(AVAL, digits = 4, as_char = TRUE, na_char = NULL),
      !is.na(AVALC) ~ AVALC
    ),
    VAL_HL = case_when(
      !is.na(ANRIND) ~ substr(ANRIND, 1, 1),
      .default = NA
    ),
    # !!!! remove EGCLSIG creation !!!!
    EGCLSIG = "Y",
    VAL_CS = case_when(
      EGCLSIG == "Y" ~ "CS",
      EGCLSIG == "N" ~ "NCS",
      .default = NA
    ),
    VAL = case_when(
      !is.na(VAL_RES) & !is.na(VAL_HL) & !is.na(VAL_CS) ~ paste(
        VAL_RES,
        VAL_HL,
        VAL_CS,
        sep = " "
      ),
      !is.na(VAL_RES) & !is.na(VAL_HL) & is.na(VAL_CS) ~ paste(
        VAL_RES,
        VAL_HL,
        sep = " "
      ),
      !is.na(VAL_RES) & is.na(VAL_HL) & !is.na(VAL_CS) ~ paste(
        VAL_RES,
        VAL_CS,
        sep = " "
      ),
      !is.na(VAL_RES) & is.na(VAL_HL) & is.na(VAL_CS) ~ VAL_RES,
      .default = NA
    ),
    # CRIT = case_when(
    #   (is.na(CRIT1) | CRIT1FL == "N") & (is.na(CRIT2) | CRIT2FL == "N")
    #     ~ "",
    #   !is.na(CRIT1) & CRIT1FL == "Y" ~ CRIT1,
    #   !is.na(CRIT2) & CRIT2FL == "Y" ~ CRIT2,
    # ),
    CRIT = case_when(
      is.na(CRIT2) | CRIT2FL == "N" ~ "",
      !is.na(CRIT2) & CRIT2FL == "Y" ~ CRIT2,
    ),
    COL0 = explicit_na(.data[[trtvar]], ""),
    COL1 = explicit_na(USUBJID, ""),
    COL2 = paste(AGE, SEX, RACE_DECODE, sep = concat_sep),
    COL3 = explicit_na(PARAM, ""),
    # Optional Variable: ATM
    COL4 = paste(ADT, concat_sep, ATM, " (", ADY, ")", sep = ""),
    COL5 = explicit_na(AVISIT, ""),
    # Optional Column: COL6/ATPT
    COL6 = explicit_na(ATPT, ""),
    COL7 = explicit_na(VAL, ""),
    # Optional Column: COL8/CHG
    COL8 = case_when(
      is.na(CHG) ~ "",
      is.na(VALDIGMAX) & !is.na(AVAL) & !is.na(CHG) ~
        tidytlg::roundSAS(CHG, digits = 0, as_char = TRUE, na_char = NULL),
      VALDIGMAX == 0 & !is.na(CHG) ~
        tidytlg::roundSAS(CHG, digits = 0, as_char = TRUE, na_char = NULL),
      VALDIGMAX == 1 & !is.na(CHG) ~
        tidytlg::roundSAS(CHG, digits = 1, as_char = TRUE, na_char = NULL),
      VALDIGMAX == 2 & !is.na(CHG) ~
        tidytlg::roundSAS(CHG, digits = 2, as_char = TRUE, na_char = NULL),
      VALDIGMAX == 3 & !is.na(CHG) ~
        tidytlg::roundSAS(CHG, digits = 3, as_char = TRUE, na_char = NULL),
      VALDIGMAX >= 4 & !is.na(CHG) ~
        tidytlg::roundSAS(CHG, digits = 4, as_char = TRUE, na_char = NULL)
    ),
    # Optional Column: COL9/CRITy
    COL9 = explicit_na(CRIT, "")
  ) %>%
  arrange(COL0, COL1, COL2, COL3, !is.na(ADYN), ADYN, ADTM)

lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  COL2 = paste("Age (years)", "Sex", "Race", sep = concat_sep),
  COL3 = "ECG Parameter (unit)",
  # Optional Variable: ATM
  COL4 = paste(
    "Assessment Date",
    "Time (Study Day~[super a])",
    sep = concat_sep
  ),
  COL5 = "Visit",
  # Optional Column: COL6/ATPT
  COL6 = "Time Point",
  COL7 = "Result",
  # Optional Column: COL8/CHG
  COL8 = "Change from Baseline",
  # Optional Column: COL9/CRITy
  COL9 = "Criteria"
)

###############################################################################
# Build listing
###############################################################################

result <- rlistings::as_listing(
  df = lsting,
  key_cols = key_cols,
  disp_cols = disp_cols
)

###############################################################################
# Add titles and footnotes
###############################################################################

result <- set_titles(result, tab_titles)

###############################################################################
# Output listing
###############################################################################

tt_to_tlgrtf(head(result, 100), file = fileid, orientation = "landscape")
