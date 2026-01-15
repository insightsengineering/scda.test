###############################################################################
## Original Reporting Effort: Standards
## Program Name:              lsiex03.R
## R version:                 4.2.1
## Short Description:         Create LSIEX03: Listing of Study Treatment
##                            Administration for Infusions
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      2024-01-19
## Input:                     ADEX
## Output:                    lsiex03.rtf
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

###############################################################################
# Define script level parameters
###############################################################################

tblid <- "LSIEX03"
fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01A"
key_cols <- c("COL0", "COL1", "COL2")
disp_cols <- paste0("COL", 0:11)
concat_sep <- " / "
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)


###############################################################################
# Process data
###############################################################################

adex <- adex_jnj %>%
  filter(!!rlang::sym(popfl) == "Y")

lsting <- adex %>%
  mutate(
    AACTDU1 = stringr::str_to_sentence(AACTDU1),
    AACTDU2 = stringr::str_to_sentence(AACTDU2),
    AACTDU3 = stringr::str_to_sentence(AACTDU3),
    AACTDU4 = stringr::str_to_sentence(AACTDU4),
    AACTDU5 = stringr::str_to_sentence(AACTDU5)
  ) %>%
  unite(
    "dact",
    AACTDU1,
    AACTDU2,
    AACTDU3,
    AACTDU4,
    AACTDU5,
    sep = ", ",
    na.rm = TRUE,
    remove = FALSE
  ) %>%
  mutate(
    dact = ifelse(dact == "", NA, dact),
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
    AENDYN = ifelse(!is.na(AENDY), AENDY, NA),
    AENDY = ifelse(!is.na(AENDY), AENDY, ""),
    preas = case_when(
      toupper(AADJP) == "ADVERSE EVENT" ~ stringr::str_to_sentence(AADJP),
      toupper(AADJP) == "OTHER" ~
        paste0(
          stringr::str_to_sentence(AADJP),
          ": ",
          stringr::str_to_sentence(AADJPOTH)
        ),
      is.na(AADJP) & !is.na(AACTPR) ~ "N/A"
    ),
    dreas = case_when(
      toupper(AADJ) == "ADVERSE EVENT" ~ stringr::str_to_sentence(AADJ),
      toupper(AADJ) == "OTHER" ~
        paste0(
          stringr::str_to_sentence(AADJ),
          ": ",
          stringr::str_to_sentence(AADJOTH)
        ),
      is.na(AADJ) & !is.na(dact) ~ "N/A"
    ),
    COL0 = explicit_na(.data[[trtvar]], ""),
    COL1 = explicit_na(USUBJID, ""),
    COL2 = paste(AGE, SEX, RACE_DECODE, sep = concat_sep),
    # Optional Column: COL3/AVISIT
    COL3 = explicit_na(AVISIT, ""),
    # Optional Column: COL4/ASCHDOSE/ASCHDOSU
    COL4 = ifelse(is.na(ASCHDOSE), "", paste(ASCHDOSE, ASCHDOSU)),
    COL5 = ifelse(is.na(ADOSE), "", paste(ADOSE, ADOSU)),
    # Optional Column: COL6/ATVINF/ATVINFU
    COL6 = ifelse(is.na(ATVINF), "", paste(ATVINF, ATVINFU)),
    # Optional Column: COL7/AINFRAT/AINFRAU
    COL7 = ifelse(is.na(AINFRAT), "", paste(AINFRAT, AINFRAU)),
    COL8 = case_when(
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
    COL9 = case_when(
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
    # Optional Column: COL10/AACTPR/AADJP/AADJPOTH
    COL10 = case_when(
      !is.na(AACTPR) ~ paste(AACTPR_DECODE, preas, sep = concat_sep),
      is.na(AACTPR) & !is.na(preas) ~ paste("", preas, sep = concat_sep),
      TRUE ~ ""
    ),
    # Optional Column: COL11/AACTDU1-AACTDU5/AADJ/AADJOTH
    COL11 = case_when(
      !is.na(dact) ~ paste(dact, dreas, sep = concat_sep),
      is.na(dact) & !is.na(dreas) ~ paste("", dreas, sep = concat_sep),
      TRUE ~ ""
    )
  ) %>%
  arrange(
    COL0,
    COL1,
    COL2,
    !is.na(ASTDYN),
    ASTDYN,
    ASTDTM,
    !is.na(AENDYN),
    AENDYN,
    AENDTM
  )

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
  # Optional Column: COL6/ATVINF/ATVINFU
  COL6 = "Total Volume Infused (unit)",
  # Optional Column: COL7/AINFRAT/AINFRAU
  COL7 = "Infusion Rate (unit)",
  COL8 = paste("Start Date", "Time (Study Day~[super a])", sep = concat_sep),
  COL9 = paste("End Date", "Time (Study Day~[super a])", sep = concat_sep),
  # Optional Column: COL10/AACTPR/AADJP/AADJPOTH
  COL10 = paste("Action Taken", "Reason Prior to Infusion", sep = concat_sep),
  # Optional Column: COL11/AACTDU1-AACTDU5/AADJ/AADJOTH
  COL11 = paste("Action Taken", "Reason During Infusion", sep = concat_sep)
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
