###############################################################################
## Original Reporting Effort: Standards
## Program Name:              lsiex01.R
## R version:                 4.2.1
## Short Description:         Create LSIEX01: Listing of Study Treatment
##                            Administration
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      2024-01-18
## Input:                     ADEX
## Output:                    lsiex01.rtf
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

tblid <- "LSIEX01"
fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01A"
key_cols <- c("COL0", "COL1", "COL2")
disp_cols <- paste0("COL", 0:10)
concat_sep <- " / "
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


###############################################################################
# Process data
###############################################################################

adex <- adex_jnj %>%
  filter(!!rlang::sym(popfl) == "Y")

lsting <- adex %>%
  mutate(
    AGE = explicit_na(as.character(AGE), ""),
    SEX = explicit_na(SEX, ""),
    RACE_DECODE = explicit_na(RACE_DECODE, ""),
    ASTDT = ifelse(
      !is.na(ASTDT) & nchar(as.character(ASTDT)) == 10,
      toupper(format(ASTDT, "%d%b%Y")),
      ""
    ),
    ASTTM = ifelse(!is.na(ASTDTM), substr(as.character(ASTDTM), 12, 16), ""),
    ASTDYN = ifelse(!is.na(ASTDY), ASTDY, NA),
    ASTDY = ifelse(!is.na(ASTDY), ASTDY, ""),
    AENDT = ifelse(
      !is.na(AENDT) & nchar(as.character(AENDT)) == 10,
      toupper(format(AENDT, "%d%b%Y")),
      ""
    ),
    AENTM = ifelse(!is.na(AENDTM), substr(as.character(AENDTM), 12, 16), ""),
    AENDY = ifelse(!is.na(AENDY), AENDY, ""),
    AREASOC = explicit_na(AREASOC, ""),
    AREASOO = explicit_na(AREASOO, ""),
    AADJ = explicit_na(AADJ, ""),
    AADJOTH = explicit_na(AADJOTH, ""),
    COL0 = explicit_na(.data[[trtvar]], ""),
    COL1 = explicit_na(USUBJID, ""),
    COL2 = paste(AGE, SEX, RACE_DECODE, sep = concat_sep),
    # Optional Column: COL3/AVISIT
    COL3 = explicit_na(AVISIT, ""),
    # Optional Column: COL4/ASCHDOSE/ASCHDOSU
    COL4 = ifelse(is.na(ASCHDOSE), "", paste(ASCHDOSE, ASCHDOSU)),
    COL5 = ifelse(is.na(ADOSE), "", paste(ADOSE, ADOSU)),
    # Optional Column: COL6/ADOSFRM/ADOSFRQ/AROUTE
    COL6 = paste(
      stringr::str_to_sentence(ADOSFRM),
      ADOSFRQ,
      stringr::str_to_sentence(AROUTE),
      sep = concat_sep
    ),
    # Optional Variable: ASTTM
    COL7 = case_when(
      ASTDT == "" ~ "",
      ASTDT != "" & ASTTM != "" & ASTDY != "" ~
        paste0(ASTDT, concat_sep, ASTTM, " (", ASTDY, ")"),
      ASTDT != "" & ASTTM == "" & ASTDY != "" ~
        paste0(ASTDT, concat_sep, "--:--", " (", ASTDY, ")"),
      ASTDT != "" & ASTTM != "" & ASTDY == "" ~
        paste0(ASTDT, concat_sep, ASTTM, " (-)"),
      ASTDT != "" & ASTTM == "" & ASTDY == "" ~
        paste0(ASTDT, concat_sep, "--:--", " (-)"),
    ),
    # Optional Variable: AENTM
    COL8 = case_when(
      AENDT == "" ~ "",
      AENDT != "" & AENTM != "" & AENDY != "" ~
        paste0(AENDT, concat_sep, AENTM, " (", AENDY, ")"),
      AENDT != "" & AENTM == "" & AENDY != "" ~
        paste0(AENDT, concat_sep, "--:--", " (", AENDY, ")"),
      AENDT != "" & AENTM != "" & AENDY == "" ~
        paste0(AENDT, concat_sep, AENTM, " (-)"),
      AENDT != "" & AENTM == "" & AENDY == "" ~
        paste0(AENDT, concat_sep, "--:--", " (-)"),
    ),
    # Optional Column: COL9/AREASOC/AREASOO
    COL9 = case_when(
      toupper(AREASOC) == "OTHER" ~
        paste(
          stringr::str_to_sentence(AREASOC),
          stringr::str_to_sentence(AREASOO),
          sep = ": "
        ),
      !is.na(AREASOC) ~ stringr::str_to_sentence(AREASOC),
      is.na(AREASOC) ~ ""
    ),
    # Optional Column: COL10/AADJ/AADJOTH
    COL10 = case_when(
      toupper(AADJ) == "OTHER" ~
        paste(
          stringr::str_to_sentence(AADJ),
          stringr::str_to_sentence(AADJOTH),
          sep = ": "
        ),
      !is.na(AADJ) ~ stringr::str_to_sentence(AADJ),
      is.na(AADJ) ~ ""
    )
  ) %>%
  arrange(COL0, COL1, COL2, !is.na(ASTDYN), ASTDYN, ASTDTM, AVISITN)

lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  COL2 = paste("Age (years)", "Sex", "Race", sep = concat_sep),
  # Optional Column: COL3/AVISIT
  COL3 = "Visit",
  # Optional Column: COL4/ASCHDOSE/ASCHDOSU
  COL4 = "Prescribed Dose Level (unit)",
  COL5 = "Dose (unit)",
  # Optional Column: COL6/ADOSFRM/ADOSFRQ/AROUTE
  COL6 = paste("Formulation", "Frequency", "Route", sep = concat_sep),
  # Optional Variable: ASTTM
  COL7 = paste("Start Date", "Time (Study Day~[super a])", sep = concat_sep),
  # Optional Variable: AENTM
  COL8 = paste("End Date", "Time (Study Day~[super a])", sep = concat_sep),
  # Optional Column: COL9/AREASOC/AREASOO
  COL9 = "Reason Dose Not Administered, if Applicable",
  # Optional Column: COL10/AADJ/AADJOTH
  COL10 = "Reason Dose Adjusted, if Applicable"
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
