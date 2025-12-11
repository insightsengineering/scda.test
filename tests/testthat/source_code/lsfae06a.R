###############################################################################
## Original Reporting Effort: Standards
## Program Name:              lsfae06a.R
## R version:                 4.2.1
## Short Description:         Create LSFAE06a: Selected [Narrow / Broad] OCMQs
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      2024-07-21
## Input:                     ADAEOCMQ
## Output:                    lsfae06a.rtf
## Remarks:
## R-functions:
## R-function Sample Call:
##
## Modification History:
##  Rev #: 1
##  Modified By:
##  Reporting Effort: Code Refactoring
##  Date: 2025-08-13
##  Description: Removed source(read_path(cl, ...)) lines and added appropriate library imports
##  Rev #: 2
##  Modified By:
##  Reporting Effort: JJCS_TLGS_R
##  Date: 2024-05-15
##  Description: Added option to display time in start and end dates
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
library(rlistings)

###############################################################################
# Define script level parameters
###############################################################################

tblid <- "LSFAE06a"
fileid <- write_path(opath, tblid)
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")
popfl <- "SAFFL"
trtvar <- "TRT01A"
ocmqclass <- "Narrow"
key_cols <- c("COL0", "COL1", "COL2", "COL3")
disp_cols <- paste0("COL", 0:8)
concat_sep <- " / "
# Parameter to control whether time should be displayed
include_time <- TRUE

###############################################################################
# Process data
###############################################################################

adaeocmq <- adaeocmq_jnj |>
  filter(
    !!sym(popfl) == "Y" &
      !is.na(OCMQNAM) &
      OCMQCLSS == ocmqclass &
      TRTEMFL == "Y"
  )

lsting <- adaeocmq |>
  mutate(
    AGE = explicit_na(as.character(AGE), ""),
    SEX = explicit_na(SEX, ""),
    RACE_DECODE = explicit_na(RACE_DECODE, ""),
    AEDECOD = explicit_na(AEDECOD, ""),
    AETERM = ifelse(
      is.na(AETERM),
      "",
      string_to_title(gsub("\\$", "", AETERM))
    ),
    ASTDT = ifelse(
      nchar(as.character(ASTDT)) == 10,
      toupper(format(ASTDT, "%d%b%Y")),
      ""
    ),
    ASTTM = ifelse(
      include_time & !is.na(ASTDTM),
      substr(as.character(ASTDTM), 12, 16),
      ""
    ),
    ASTDYN = ifelse(!is.na(ASTDY), ASTDY, NA),
    ASTDY = ifelse(!is.na(ASTDY), ASTDY, ""),
    ASTDTFS = ifelse(!is.na(ASTDTF), "*", ""),
    AENDT = explicit_na(as.character(AENDT), ""),
    AENDT = ifelse(
      nchar(AENDT) == 10,
      toupper(format(as.Date(AENDT), "%d%b%Y")),
      ""
    ),
    AENTM = ifelse(
      include_time & !is.na(AENDTM),
      substr(as.character(AENDTM), 12, 16),
      ""
    ),
    AENDY = ifelse(!is.na(AENDY), AENDY, ""),
    AENDTFS = ifelse(!is.na(AENDTF), "*", ""),
    AEOUTC = explicit_na(AEOUT_DECODE, ""),
    AESEV = explicit_na(AESEV, ""),
    AESER_DECODE = explicit_na(AESER_DECODE, ""),
    COL0 = OCMQNAM,
    COL1 = explicit_na(.data[[trtvar]], ""),
    COL2 = explicit_na(USUBJID, ""),
    COL3 = paste(AGE, SEX, RACE_DECODE, sep = concat_sep),
    COL4 = paste(AEDECOD, AETERM, sep = concat_sep),
    COL5 = case_when(
      ASTDT == "" ~ "",
      include_time & ASTDT != "" & ASTTM != "" & ASTDY != "" & ASTDTFS != "" ~
        paste0(ASTDT, concat_sep, ASTTM, " (", ASTDY, ")", ASTDTFS),
      include_time & ASTDT != "" & ASTTM != "" & ASTDY != "" & ASTDTFS == "" ~
        paste0(ASTDT, concat_sep, ASTTM, " (", ASTDY, ")"),
      include_time & ASTDT != "" & ASTTM == "" & ASTDY != "" & ASTDTFS != "" ~
        paste0(ASTDT, concat_sep, "--:--", " (", ASTDY, ")", ASTDTFS),
      include_time & ASTDT != "" & ASTTM == "" & ASTDY != "" & ASTDTFS == "" ~
        paste0(ASTDT, concat_sep, "--:--", " (", ASTDY, ")"),
      ASTDT != "" & ASTDY != "" & ASTDTFS != "" ~
        paste0(ASTDT, " (", ASTDY, ")", ASTDTFS),
      ASTDT != "" & ASTDY != "" & ASTDTFS == "" ~
        paste0(ASTDT, " (", ASTDY, ")"),
    ),
    COL6 = case_when(
      AENDT == "" ~ "",
      include_time & AENDT != "" & AENTM != "" & AENDY != "" & AENDTFS != "" ~
        paste0(AENDT, concat_sep, AENTM, " (", AENDY, ")", AENDTFS),
      include_time & AENDT != "" & AENTM != "" & AENDY != "" & AENDTFS == "" ~
        paste0(AENDT, concat_sep, AENTM, " (", AENDY, ")"),
      include_time & AENDT != "" & AENTM == "" & AENDY != "" & AENDTFS != "" ~
        paste0(AENDT, concat_sep, "--:--", " (", AENDY, ")", AENDTFS),
      include_time & AENDT != "" & AENTM == "" & AENDY != "" & AENDTFS == "" ~
        paste0(AENDT, concat_sep, "--:--", " (", AENDY, ")"),
      AENDT != "" & AENDY != "" & AENDTFS != "" ~
        paste0(AENDT, " (", AENDY, ")", AENDTFS),
      AENDT != "" & AENDY != "" & AENDTFS == "" ~
        paste0(AENDT, " (", AENDY, ")"),
    ),
    COL7 = explicit_na(AEACN_DECODE, ""),
    COL8 = paste(AEOUTC, AESEV, AESER_DECODE, sep = concat_sep)
  ) |>
  arrange(
    COL0,
    COL1,
    COL2,
    !is.na(ASTDYN),
    ASTDYN,
    if (include_time) ASTDTM else ASTDT,
    AEDECOD,
    AETERM
  )

lsting <- var_relabel(
  lsting,
  COL0 = "OCMQ",
  COL1 = "Treatment Group",
  COL2 = "Subject ID",
  COL3 = paste("Age (years)", "Sex", "Race", sep = concat_sep),
  COL4 = paste("Preferred Term", "Reported Term", sep = concat_sep),
  COL5 = if (include_time) {
    paste("Start Date", "Time (Study Day~[super a])", sep = concat_sep)
  } else {
    "Start Date (Study Day~[super a])"
  },
  COL6 = if (include_time) {
    paste("End Date", "Time (Study Day~[super a])", sep = concat_sep)
  } else {
    "End Date (Study Day~[super a])"
  },
  COL7 = "Action Taken With Study Treatment",
  COL8 = paste("Outcome", "Severity", "Serious", sep = concat_sep)
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
# Add titles and footnotes:
###############################################################################

result <- set_titles(result, tab_titles)

###############################################################################
# Output listing
###############################################################################

tt_to_tlgrtf(result, file = fileid, orientation = "landscape")
