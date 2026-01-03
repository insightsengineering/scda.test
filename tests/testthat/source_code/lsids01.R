###############################################################################
## Original Reporting Effort: Standards
## Program Name:              lsids01.R
## R version:                 4.2.1
## Short Description:         Create LSIDS01: Listing of Subjects Who
##                            Permanently Discontinued Treatment
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      2024-01-08
## Input:                     ADSL, DS
## Output:                    lsids01.rtf
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

tblid <- "LSIDS01"
fileid <- write_path(opath, tblid)
popfl <- "FASFL"
trtvar <- "TRT01P"
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
  filter(!!rlang::sym(popfl) == "Y" & !(EOTSTT %in% c("COMPLETED", "ONGOING")))

ds <- ds_jnj %>%
  filter(
    (DSSCAT %in% c("TREATMENT")) &
      DSCAT == "DISPOSITION EVENT" &
      DSDECOD != "COMPLETED"
  ) %>%
  select(STUDYID, USUBJID, DSSCAT)

adsl_ds <- adsl %>%
  left_join(ds, by = c("STUDYID" = "STUDYID", "USUBJID" = "USUBJID"))

adexsum <- adexsum_jnj %>%
  filter(PARAMCD == "CUMDOSE") %>%
  select(STUDYID, USUBJID, PARAMCD, PARAM, AVAL)

adsl_ds_adexsum <- left_join(
  adsl_ds,
  adexsum,
  by = c(
    "STUDYID" = "STUDYID",
    "USUBJID" = "USUBJID"
  )
)

lsting <- adsl_ds_adexsum %>%
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
    DCTREAS = explicit_na(DCTREAS, ""),
    DCTREASP = explicit_na(DCTREASP, ""),
    COL0 = explicit_na(.data[[trtvar]], ""),
    COL1 = explicit_na(USUBJID, ""),
    COL2 = paste(AGE, SEX, RACE_DECODE, sep = concat_sep),
    # Optional Column: COL3/DSSCAT
    COL3 = explicit_na(stringr::str_to_sentence(DSSCAT), ""),
    # Optional Column: COL4/LTVISIT
    COL4 = explicit_na(LTVISIT, ""),
    COL5 = explicit_na(as.character(TRTEDY), ""),
    # Optional Column: COL6/CUMDOSE/CUMDOSU
    COL6 = paste0(AVAL, " ", AVALU),
    COL7 = ifelse(
      is.na(DCTDT),
      "",
      toupper(format(as.Date(DCTDT), format = "%d%b%Y"))
    ),
    COL8 = case_when(
      DCTREAS == "OTHER" ~
        paste0(DCTREAS, " (", stringr::str_to_sentence(DCTREASP), ")"),
      DCTREAS != "OTHER" ~
        DCTREAS
    )
  ) %>%
  arrange(COL0, COL1, COL2, COL3)

lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  COL2 = paste("Age (years)", "Sex", "Race", sep = concat_sep),
  # Optional Column: COL3/DSSCAT
  COL3 = "Study Agent Discontinued",
  # Optional Column: COL4/LTVISIT
  COL4 = "Last Visit~[super a]",
  COL5 = "Study Day~[super b] of Last Study Agent Administered",
  # Optional Column: COL6/CUMDOSE/CUMDOSU
  COL6 = "Total Dose (unit)~[super c]",
  COL7 = "Date of Discontinuation",
  COL8 = "Primary Reason for Discontinuation"
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
