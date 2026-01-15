###############################################################################
## Original Reporting Effort: Standards
## Program Name:              lsids03.R
## R version:                 4.2.1
## Short Description:         Create LSIDS03: Listing of Subjects Who Were
##                            Unblinded During the Study
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      2024-01-12
## Input:                     ADSL, ADEXSUM
## Output:                    lsids03.rtf
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
library(stringi)

###############################################################################
# Define script level parameters
###############################################################################

tblid <- "LSIDS03"
fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01P"
key_cols <- c("COL0", "COL1")
disp_cols <- paste0("COL", 0:9)
concat_sep <- " / "
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


###############################################################################
# Process data
###############################################################################

adsl <- adsl_jnj %>%
  filter(!!rlang::sym(popfl) == "Y" & UNBLNDFL == "Y")

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
    EOTSTT = explicit_na(EOTSTT, ""),
    EOSSTT = explicit_na(EOSSTT, ""),
    COL0 = explicit_na(.data[[trtvar]], ""),
    COL1 = explicit_na(USUBJID, ""),
    COL2 = paste(AGE, SEX, RACE_DECODE, sep = concat_sep),
    # Optional Column: COL3/LTVISIT
    COL3 = explicit_na(LTVISIT, ""),
    COL4 = explicit_na(as.character(UNBLNDDY), ""),
    COL5 = explicit_na(as.character(TRTEDY), ""),
    # Optional Column: COL6/CUMDOSE/CUMDOSU
    COL6 = paste0(AVAL, " ", AVALU),
    COL7 = explicit_na(stringi::stri_trans_totitle(UNBREAS), ""),
    COL8 = case_when(
      EOTSTT == "DISCONTINUED" ~ "Yes",
      EOTSTT != "DISCONTINUED" ~ "No"
    ),
    COL9 = case_when(
      EOSSTT == "DISCONTINUED" ~ "Yes",
      EOSSTT != "DISCONTINUED" ~ "No"
    )
  ) %>%
  arrange(COL0, COL1)

lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  COL2 = paste("Age (years)", "Sex", "Race", sep = concat_sep),
  # Optional Column: COL3/LTVISIT
  COL3 = "Last Visit~[super a]",
  COL4 = "Study Day~[super b] of Unblinding",
  COL5 = "Study Day~[super b] of Last Study Agent Administered",
  # Optional Column: COL6/CUMDOSE/CUMDOSU
  COL6 = "Total Dose (unit)~[super c]",
  COL7 = "Reason for Unblinding",
  COL8 = "Was Study Agent Discontinued?",
  COL9 = "Was Study Participation Discontinued Prematurely?"
)

###############################################################################
# Build listing
###############################################################################

result <- rlistings::as_listing(
  df = lsting,
  key_cols = key_cols,
  disp_cols = disp_cols,
)

###############################################################################
# Add titles and footnotes
###############################################################################

result <- set_titles(result, tab_titles)

###############################################################################
# Output listing
###############################################################################

tt_to_tlgrtf(head(result, 100), file = fileid, orientation = "landscape")
