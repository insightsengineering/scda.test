###############################################################################
## Original Reporting Effort: Standards
## Program Name:              lsfvit02.R
## R version:                 4.2.1
## Short Description:         Create LSFVIT02: Listing of Vital Signs
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      2024-09-12
## Input:                     ADVS
## Output:                    lsfvit02.rtf
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
library(tidyr)
library(rtables)
library(rlistings)
library(junco)
library(tidytlg)

###############################################################################
# Define script level parameters
###############################################################################

tblid <- "LSFVIT02"
fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01A"
key_cols <- c("COL0", "COL1", "COL2", "COL3")
disp_cols <- paste0("COL", 0:9)
concat_sep <- " / "
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


###############################################################################
# Process data
###############################################################################

advs <- advs_jnj %>%
  filter(!!rlang::sym(popfl) == "Y" & PARAMCD != "VSALL")

advs_dig <- tidytlg:::make_precision_data(
  df = advs,
  decimal = 4,
  precisionby = "PARAMCD",
  precisionon = "AVAL"
) %>%
  rename(c(VALDIGMAX = "decimal"))

advs_list <- advs %>%
  inner_join(advs_dig, by = c("PARAMCD" = "PARAMCD"))

lsting <- advs_list %>%
  mutate(
    AGE = explicit_na(as.character(AGE), ""),
    SEX = explicit_na(SEX, ""),
    RACE_DECODE = explicit_na(RACE_DECODE, ""),
    ADT = ifelse(
      nchar(as.character(ADT)) == 10,
      toupper(format(ADT, "%d%b%Y")),
      ""
    ),
    ATM = ifelse(!is.na(ADTM), substr(as.character(ADTM), 12, 16), ""),
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
    VAL = case_when(
      !is.na(VAL_RES) & !is.na(VAL_HL) ~ paste(VAL_RES, VAL_HL, sep = " "),
      !is.na(VAL_RES) & is.na(VAL_HL) ~ VAL_RES,
      .default = NA
    ),
    CRIT1L = ifelse(CRIT1FL == "Y", as.character(CRIT1), NA),
    CRIT2L = ifelse(CRIT2FL == "Y", as.character(CRIT2), NA),
    CRIT3L = ifelse(CRIT3FL == "Y", as.character(CRIT3), NA)
  ) %>%
  unite(
    "CRITL",
    CRIT1L,
    CRIT2L,
    CRIT3L,
    sep = ", ",
    na.rm = TRUE,
    remove = FALSE
  ) %>%
  mutate(
    COL0 = explicit_na(.data[[trtvar]], ""),
    COL1 = explicit_na(USUBJID, ""),
    COL2 = paste(AGE, SEX, RACE_DECODE, sep = concat_sep),
    COL3 = explicit_na(PARAM, ""),
    # Optional Variable: ATM
    COL4 = case_when(
      ADT == "" ~ "",
      ADT != "" & ATM != "" & ADY != "" ~
        paste0(ADT, concat_sep, ATM, " (", ADY, ")"),
      ADT != "" & ATM == "" & ADY != "" ~
        paste0(ADT, concat_sep, "--:--", " (", ADY, ")"),
      ADT != "" & ATM != "" & ADY == "" ~
        paste0(ADT, concat_sep, ATM, " (-)"),
      ADT != "" & ATM == "" & ADY == "" ~
        paste0(ADT, concat_sep, "--:--", " (-)"),
    ),
    COL5 = explicit_na(AVISIT, ""),
    # Optional Column: COL6/ATPT
    COL6 = explicit_na(ATPT, ""),
    COL7 = VAL,
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
    # Optional Column: COL9/CRITy/ATOXGR
    COL9 = explicit_na(CRITL, ""),
    # COL9 = explicit_na(ATOXGR, ""),
  ) %>%
  arrange(COL0, COL1, COL2, COL3, !is.na(ADYN), ADYN, ADTM)

lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  COL2 = paste("Age (years)", "Sex", "Race", sep = concat_sep),
  COL3 = "Vital Sign (unit)",
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
  COL8 = "Change From Baseline",
  # Optional Column: COL9/CRITy/ATOXGR
  COL9 = "Criteria"
  # COL9 = "Grade"
)

# Now create a dummy dataframe with only one row per subject ID
lsting <- lsting %>%
  group_by(USUBJID) %>%
  slice(n()) %>%
  ungroup()

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



tt_to_tlgrtf( 
  result,
  file = paste0(fileid),
  orientation = "landscape"
)
