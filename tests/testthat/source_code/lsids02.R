###############################################################################
## Original Reporting Effort: Standards
## Program Name:              lsids02.R
## R version:                 4.2.1
## Short Description:         Create LSIDS02: Listing of Subjects Who
##                            Discontinued Study Participation Prematurely
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      2024-01-12
## Input:                     ADSL
## Output:                    lsids02.rtf
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

tblid <- "LSIDS02"
fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01P"
key_cols <- c("COL0", "COL1")
disp_cols <- paste0("COL", 0:10)
concat_sep <- " / "
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

###############################################################################
# Process data
###############################################################################

adsl <- adsl_jnj %>%
  filter(!!rlang::sym(popfl) == "Y" & EOSSTT %in% c("DISCONTINUED"))

adexsum <- adexsum_jnj %>%
  filter(PARAMCD == "CUMDOSE") %>%
  select(STUDYID, USUBJID, PARAMCD, PARAM, AVAL)

adsl_adexsum <- left_join(
  adsl,
  adexsum,
  by = c(
    "STUDYID" = "STUDYID",
    "USUBJID" = "USUBJID"
  )
)

lsting <- adsl_adexsum %>%
  mutate(
    AGE = explicit_na(as.character(AGE), ""),
    SEX = explicit_na(SEX, ""),
    RACE_DECODE = explicit_na(RACE_DECODE, ""),
    AVAL = explicit_na(as.character(AVAL), ""),
    AVALU = case_when(
      !is.na(AVAL) ~
        stringr::str_extract(PARAM, "(?<=\\()([^()]*?)(?=\\)[^()]*$)"),
      is.na(AVAL) ~
        ""
    ),
    DCSREAS = explicit_na(DCSREAS, ""),
    DCSREASP = explicit_na(DCSREASP, ""),
    COL0 = explicit_na(.data[[trtvar]], ""),
    COL1 = explicit_na(USUBJID, ""),
    COL2 = paste(AGE, SEX, RACE_DECODE, sep = concat_sep),
    # Optional Column: COL3/LSVISIT
    COL3 = explicit_na(LSVISIT, ""),
    COL4 = explicit_na(as.character(EOSDY), ""),
    # Optional Column: COL5/LTVISIT
    COL5 = explicit_na(LTVISIT, ""),
    # Optional Column: COL6/TRTEDY
    COL6 = explicit_na(as.character(TRTEDY), ""),
    # Optional Column: COL7/CUMDOSE/CUMDOSU
    COL7 = paste0(AVAL, " ", AVALU),
    COL8 = ifelse(
      is.na(EOSDT),
      "",
      toupper(format(as.Date(EOSDT), format = "%d%b%Y"))
    ),
    COL9 = case_when(
      DCSREAS == "OTHER" ~
        paste0(DCSREAS, " (", stringr::str_to_sentence(DCSREASP), ")"),
      DCSREAS != "OTHER" ~
        DCSREAS
    ),
    # Optional Column: COL10/UNBLNDFL
    COL10 = ifelse(is.na(UNBLNDFL), "No", "Yes")
  ) %>%
  arrange(COL0, COL1)

lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  COL2 = paste("Age (years)", "Sex", "Race", sep = concat_sep),
  # Optional Column: COL3/LSVISIT
  COL3 = "Last Study Visit~[super a]",
  COL4 = "Study Day~[super b] of Discontinuation",
  # Optional Column: COL5/LTVISIT
  COL5 = "Last Treatment Visit~[super c]",
  # Optional Column: COL6/TRTEDY
  COL6 = "Study Day~[super b] of Last Study Agent Administered",
  # Optional Column: COL7/CUMDOSE/CUMDOSU
  COL7 = "Total Dose (unit)~[super d]",
  COL8 = "Date of Discontinuation",
  COL9 = "Primary Reason for Discontinuation",
  # Optional Column: COL10/UNBLNDFL
  COL10 = "Was Blind Broken?"
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
