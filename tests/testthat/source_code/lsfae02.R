###############################################################################
## Original Reporting Effort: Standards
## Program Name:              lsfae02.R
## R version:                 4.2.1
## Short Description:         Create LSFAE02: Listing of Treatment-emergent
##                            Serious Adverse Events
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      2024-01-22
## Input:                     ADAE
## Output:                    lsfae02.rtf
## Remarks:
## R-functions:
## R-function Sample Call:
##
## Modification History:
##  Rev #: 1
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
library(stringi)

###############################################################################
# Define script level parameters
###############################################################################

tblid <- "LSFAE02"
fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01A"
key_cols <- c("COL0", "COL1", "COL2")
disp_cols <- paste0("COL", 0:10)
concat_sep <- " / "
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")
# Parameter to control whether time should be displayed
include_time <- TRUE

###############################################################################
# Process data
###############################################################################

adae <- adae_jnj %>%
  filter(!!rlang::sym(popfl) == "Y" & TRTEMFL == "Y" & AESER == "Y")

lsting <- adae %>%
  mutate(
    AGE = explicit_na(as.character(AGE), ""),
    SEX = explicit_na(SEX, ""),
    RACE_DECODE = explicit_na(RACE_DECODE, ""),
    DOSEON = explicit_na(as.character(DOSEON), ""),
    DOSEU = explicit_na(DOSEU, ""),
    DOSEDY = explicit_na(as.character(DOSEDY), ""),
    AEDECOD = explicit_na(AEDECOD, ""),
    AETERM = ifelse(
      is.na(AETERM),
      "",
      stringi::stri_trans_totitle(gsub("\\$", "", AETERM))
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
    ADURN = explicit_na(as.character(ADURN), ""),
    AEOUTC = explicit_na(AEOUT_DECODE, ""),
    # Optional Variable: COL10/AESEV/AETOXGR
    # AESEV = explicit_na(AESEV,""),
    AETOXGR = explicit_na(AETOXGR, ""),
    COL0 = explicit_na(.data[[trtvar]], ""),
    COL1 = explicit_na(USUBJID, ""),
    COL2 = paste(AGE, SEX, RACE_DECODE, sep = concat_sep),
    COL3 = paste(paste0(DOSEON, " ", DOSEU), DOSEDY, sep = concat_sep),
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
    COL7 = explicit_na(ADURN, ""),
    # Optional Column: COL8/AEACN
    COL8 = explicit_na(AEACN_DECODE, ""),
    COL9 = explicit_na(AEREL_DECODE, ""),
    # Optional Variable: COL10/AESEV/AETOXGR
    # COL10 = paste(AEOUTC, AESEV, sep = concat_sep)
    COL10 = paste(AEOUTC, AETOXGR, sep = concat_sep)
  ) %>%
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
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  COL2 = paste("Age (years)", "Sex", "Race", sep = concat_sep),
  COL3 = paste(
    "Onset Dose (unit)",
    "Day of Last Dose~[super a,b]",
    sep = concat_sep
  ),
  COL4 = paste("Preferred Term", "Reported Term", sep = concat_sep),
  COL5 = if (include_time) {
    paste("Start Date", "Time (Study Day~[super b])", sep = concat_sep)
  } else {
    "Start Date (Study Day~[super b])"
  },
  COL6 = if (include_time) {
    paste("End Date", "Time (Study Day~[super b])", sep = concat_sep)
  } else {
    "End Date (Study Day~[super b])"
  },
  COL7 = "AE Duration (Days)",
  # Optional Column: COL8/AEACN
  COL8 = "Action Taken With Study Treatment",
  COL9 = "Relationship to Study Treatment~[super c]",
  # Optional Variable: COL10/AESEV/AETOXGR
  # COL10 = paste("Outcome", "Severity", sep = concat_sep)
  COL10 = paste("Outcome", "Toxicity Grade", sep = concat_sep)
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

tt_to_tlgrtf(result, file = fileid, orientation = "landscape")
