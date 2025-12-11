###############################################################################
## Original Reporting Effort: Standards
## Program Name:              lsimh01.R
## R version:                 4.2.1
## Short Description:         Create LSIMH01: Listing of [Medical
##                            History/Medical History of Interest]
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      2024-02-09
## Input:                     ADSL, MH
## Output:                    lsimh01.rtf
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

tblid <- "LSIMH01"
fileid <- write_path(opath, tblid)
popfl <- "FASFL"
trtvar <- "TRT01P"
key_cols <- c("COL0", "COL1", "COL2")
disp_cols <- paste0("COL", 0:8)
concat_sep <- " / "
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


###############################################################################
# Process data
###############################################################################

adsl <- adsl_jnj %>%
  filter(!!rlang::sym(popfl) == "Y")

mh <- mh_jnj

adsl_mh <- adsl %>%
  inner_join(mh, by = c("STUDYID", "USUBJID"))

lsting <- adsl_mh %>%
  mutate(
    AGE = explicit_na(as.character(AGE), ""),
    SEX = explicit_na(SEX, ""),
    RACE_DECODE = explicit_na(RACE_DECODE, ""),
    MHDECOD = case_when(
      MHDECOD == "" ~ paste0("Uncoded: ", MHTERM),
      .default = MHDECOD
    ),
    MHBODSYS = case_when(
      MHBODSYS == "" ~ "Uncoded",
      .default = MHBODSYS
    ),
    MHDECOD = explicit_na(MHDECOD, ""),
    MHTERM = explicit_na(MHTERM, ""),
    MHENRTPTL = ifelse(MHENRTPT == "ONGOING", "Yes", "No"),
    MHSTDT = as.Date(MHSTDTC, format = "%Y-%m-%d"),
    MHSTYR = ifelse(
      stringr::str_length(MHSTDTC) >= 4,
      substr(MHSTDTC, 1, 4),
      NA
    ),
    MHSTMO = month.abb[
      as.numeric(ifelse(
        stringr::str_length(sub("T.*", "", MHSTDTC)) >= 7 &
          substr(sub("T.*", "", MHSTDTC), 6, 7) != "--",
        substr(sub("T.*", "", MHSTDTC), 6, 7),
        NA
      ))
    ],
    MHSTDAY = ifelse(
      stringr::str_length(MHSTDTC) >= 10,
      substr(MHSTDTC, 9, 10),
      NA
    ),
    MHSTDY = ifelse(MHSTDT > TRTSDT, MHSTDT - TRTSDT + 1, MHSTDT - TRTSDT),
    MHSTDYL = ifelse(!is.na(MHSTDT) & is.na(MHSTDY), "-", MHSTDY),
    MHENDT = as.Date(MHENDTC, format = "%Y-%m-%d"),
    MHENYR = ifelse(
      stringr::str_length(MHENDTC) >= 4,
      substr(MHENDTC, 1, 4),
      NA
    ),
    MHENMO = month.abb[
      as.numeric(ifelse(
        stringr::str_length(sub("T.*", "", MHENDTC)) >= 7 &
          substr(sub("T.*", "", MHENDTC), 6, 7) != "--",
        substr(sub("T.*", "", MHENDTC), 6, 7),
        NA
      ))
    ],
    MHENDAY = ifelse(
      stringr::str_length(MHENDTC) >= 10,
      substr(MHENDTC, 9, 10),
      NA
    ),
    MHENDY = ifelse(MHENDT > TRTSDT, MHENDT - TRTSDT + 1, MHENDT - TRTSDT),
    MHENDYL = case_when(
      !is.na(MHENDY) ~ as.character(MHENDY),
      is.na(MHENDY) & !is.na(MHENDT) ~ "-",
      TRUE ~ NA
    ),
  ) %>%
  unite(
    "MHSTDTL",
    MHSTDAY,
    MHSTMO,
    MHSTYR,
    sep = "",
    na.rm = TRUE,
    remove = FALSE
  ) %>%
  unite(
    "MHENDTL",
    MHENDAY,
    MHENMO,
    MHENYR,
    sep = "",
    na.rm = TRUE,
    remove = FALSE
  ) %>%
  mutate(
    COL0 = explicit_na(.data[[trtvar]], ""),
    COL1 = explicit_na(USUBJID, ""),
    COL2 = paste(AGE, SEX, RACE_DECODE, sep = concat_sep),
    COL3 = explicit_na(MHBODSYS, ""),
    COL4 = paste(MHDECOD, MHTERM, sep = concat_sep),
    COL5 = case_when(
      MHSTDTL != "" & !is.na(MHSTDYL) ~
        paste0(toupper(MHSTDTL), " (", MHSTDYL, ")"),
      MHSTDTL != "" & is.na(MHSTDYL) ~
        paste0(toupper(MHSTDTL), ""),
      TRUE ~ ""
    ),
    COL6 = case_when(
      MHENDTL != "" & !is.na(MHENDYL) ~
        paste0(toupper(MHENDTL), " (", MHENDYL, ")"),
      MHENDTL != "" & is.na(MHENDYL) ~
        paste0(toupper(MHENDTL), ""),
      TRUE ~ ""
    ),
    COL7 = explicit_na(MHENRTPTL, ""),
    # Optional Column: COL8/MHTOXGR
    COL8 = explicit_na(MHTOXGR, ""),
  ) %>%
  arrange(COL0, COL1, COL2, MHSTDTC, MHBODSYS, MHDECOD)

lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  COL2 = paste("Age (years)", "Sex", "Race", sep = concat_sep),
  COL3 = "System Organ Class",
  COL4 = paste("[Preferred Term", "Reported Term]", sep = concat_sep),
  COL5 = "Start Date (Study Day)~[super a]",
  COL6 = "End Date (Study Day)~[super a]",
  COL7 = "Ongoing?",
  # Optional Column: COL8/MHTOXGR
  COL8 = "Toxicity Grade"
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

###############################################################################
# Output listing
###############################################################################

tt_to_tlgrtf(result, file = fileid, orientation = "landscape")
