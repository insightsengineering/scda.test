###############################################################################
## Original Reporting Effort: Standards
## Program Name:              lsfdth01.R
## R version:                 4.2.1
## Short Description:         Create LSFDTH01: Listing of Deaths
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      2024-01-12
## Input:                     ADSL, ADEXSUM
## Output:                    lsfdth01.rtf
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

###############################################################################
# Define script level parameters
###############################################################################

tblid <- "LSFDTH01"
fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01A"
key_cols <- c("COL0", "COL1")
disp_cols <- paste0("COL", 0:8)
concat_sep <- " / "
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

###############################################################################
# Process data
###############################################################################

adsl <- adsl_jnj %>%
  filter(!!rlang::sym(popfl) == "Y" & DTHFL == "Y")

adexsum <- adexsum_jnj %>%
  filter(PARAMCD == "TRTDURM") %>%
  select(STUDYID, USUBJID, AVAL)

adexsum_dig <- tidytlg:::make_precision_data(
  df = adexsum,
  decimal = 4,
  precisionby = "STUDYID",
  precisionon = "AVAL"
) %>%
  rename(c(VALDIGMAX = "decimal"))

adsl_adexsum <- adsl %>%
  inner_join(adexsum, by = c("STUDYID", "USUBJID")) %>%
  left_join(adexsum_dig, by = c("STUDYID" = "STUDYID"))

lsting <- adsl_adexsum %>%
  mutate(
    AGE = explicit_na(as.character(AGE), ""),
    SEX = explicit_na(SEX, ""),
    RACE_DECODE = explicit_na(RACE_DECODE, ""),
    LDOSE = explicit_na(as.character(LDOSE), ""),
    LDOSU = explicit_na(LDOSU, ""),
    DTHCAUS = explicit_na(stringr::str_to_sentence(DTHCAUS), ""),
    DTHTERM = explicit_na(stringr::str_to_sentence(DTHTERM), ""),
    COL0 = explicit_na(.data[[trtvar]], ""),
    COL1 = explicit_na(USUBJID, ""),
    COL2 = paste(AGE, SEX, RACE_DECODE, sep = concat_sep),
    COL3 = paste(LDOSE, LDOSU),
    COL4 = case_when(
      is.na(AVAL) ~ "",
      VALDIGMAX == 0 & !is.na(AVAL) ~
        tidytlg::roundSAS(AVAL, digits = 0, as_char = TRUE, na_char = NULL),
      VALDIGMAX >= 1 & !is.na(AVAL) ~
        tidytlg::roundSAS(AVAL, digits = 1, as_char = TRUE, na_char = NULL)
    ),
    # Optional Column: COL5/TRTEDT/TRTEDY
    COL5 = ifelse(
      is.na(TRTEDT),
      "",
      paste0(
        toupper(format(TRTEDT, "%d%b%Y")),
        " (",
        TRTEDY,
        ")"
      )
    ),
    # Optional Column: COL6/LDSTODTH
    COL6 = explicit_na(as.character(LDSTODTH), ""),
    COL7 = ifelse(
      is.na(DTHDT),
      "",
      paste0(
        toupper(format(DTHDT, "%d%b%Y")),
        " (",
        DTHDY,
        ")"
      )
    ),
    COL8 = paste(DTHCAUS, DTHTERM, sep = concat_sep)
  ) %>%
  arrange(COL0, COL1)

lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  COL2 = paste("Age (years)", "Sex", "Race", sep = concat_sep),
  COL3 = "Last Dose of Study Treatment",
  COL4 = "Duration of Treatment Months~[super a]",
  # Optional Column: COL5/TRTEDT/TRTEDY
  COL5 = "Date of Last Dose of Study Treatment (Study Day~[super b])",
  # Optional Column: COL6/LDSTODTH
  COL6 = "Days From Last Study Treatment Administration to Death~[super c]",
  COL7 = "Date of Death (Study Day~[super d])",
  COL8 = paste(
    "Primary Cause of Death (Preferred Term",
    "Verbatim",
    sep = concat_sep
  )
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
