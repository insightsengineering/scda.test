###############################################################################
## Original Reporting Effort: Standards
## Program Name:              lsicm01.R
## R version:                 4.2.1
## Short Description:         Create LSICM01: Listing of Medications
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      2024-01-17
## Input:                     ADCM
## Output:                    lsicm01.rtf
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

tblid <- "LSICM01"
fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01A"
key_cols <- c("COL0", "COL1")
disp_cols <- paste0("COL", 0:11)
concat_sep <- " / "
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


###############################################################################
# Process data
###############################################################################

adcm <- adcm_jnj %>%
  filter(
    !!rlang::sym(popfl) == "Y" &
      ((CMPRESP == "Y" & CMOCCUR == "Y") |
        (is.na(CMPRESP)))
  )

lsting <- adcm %>%
  # Update CQzzNAM variabes based on study requirements
  unite(
    "CQNAM",
    CQ01NAM,
    CQ02NAM,
    CQ03NAM,
    CQ04NAM,
    CQ05NAM,
    CQ06NAM,
    CQ07NAM,
    sep = ", ",
    na.rm = TRUE,
    remove = FALSE
  ) %>%
  mutate(across(
    c(CMLVL1:CMLVL4, CMTRT, CMDECOD, CMDOSFRQ, CMROUTE, CMINDCSP),
    ~ ifelse(. == "", "-", as.character(.))
  )) %>%
  mutate(
    CMSTDT = as.Date(CMSTDTC, format = "%Y-%m-%d"),
    CMSTYR = ifelse(
      stringr::str_length(sub("T.*", "", CMSTDTC)) >= 4 &
        substr(sub("T.*", "", CMSTDTC), 1, 4) != "----",
      substr(sub("T.*", "", CMSTDTC), 1, 4),
      NA
    ),
    CMSTMO = month.abb[as.numeric(ifelse(
      stringr::str_length(sub("T.*", "", CMSTDTC)) >= 7 &
        substr(
          sub("T.*", "", CMSTDTC),
          6,
          7
        ) !=
          "--",
      substr(sub("T.*", "", CMSTDTC), 6, 7),
      NA
    ))],
    CMSTDAY = ifelse(
      stringr::str_length(sub("T.*", "", CMSTDTC)) >= 10 &
        substr(sub("T.*", "", CMSTDTC), 9, 10) != "--",
      substr(sub("T.*", "", CMSTDTC), 9, 10),
      NA
    ),
    CMSTDY = ifelse(CMSTDT > TRTSDT, CMSTDT - TRTSDT + 1, CMSTDT - TRTSDT),
    CMSTDYL = explicit_na(as.character(CMSTDY), ""),
    CMENDT = as.Date(CMENDTC, format = "%Y-%m-%d"),
    CMENYR = ifelse(
      stringr::str_length(sub("T.*", "", CMENDTC)) >= 4 &
        substr(sub("T.*", "", CMENDTC), 1, 4) != "----",
      substr(sub("T.*", "", CMENDTC), 1, 4),
      NA
    ),
    CMENMO = month.abb[
      as.numeric(ifelse(
        stringr::str_length(sub("T.*", "", CMENDTC)) >= 7 &
          substr(sub("T.*", "", CMENDTC), 6, 7) != "--",
        substr(sub("T.*", "", CMENDTC), 6, 7),
        NA
      ))
    ],
    CMENDAY = ifelse(
      stringr::str_length(sub("T.*", "", CMENDTC)) >= 10 &
        substr(sub("T.*", "", CMENDTC), 9, 10) != "--",
      substr(sub("T.*", "", CMENDTC), 9, 10),
      NA
    ),
    CMENDY = ifelse(CMENDT > TRTSDT, CMENDT - TRTSDT + 1, CMENDT - TRTSDT),
    CMENDYL = explicit_na(as.character(CMENDY), ""),
    PREFLL = ifelse(is.na(PREFL), NA, "P"),
    ONTRTFLL = ifelse(is.na(ONTRTFL), NA, "C"),
    FUPFLL = ifelse(is.na(FUPFL), NA, "F")
  ) %>%
  unite(
    "CMSTDTL",
    CMSTDAY,
    CMSTMO,
    CMSTYR,
    sep = "",
    na.rm = TRUE,
    remove = FALSE
  ) %>%
  unite(
    "CMENDTL",
    CMENDAY,
    CMENMO,
    CMENYR,
    sep = "",
    na.rm = TRUE,
    remove = FALSE
  ) %>%
  unite(
    "POF",
    PREFLL,
    ONTRTFLL,
    FUPFLL,
    sep = ";",
    na.rm = TRUE,
    remove = FALSE
  ) %>%
  mutate(
    STDATE = case_when(
      CMSTDTL != "" & CMSTDYL != "" ~
        paste0(toupper(CMSTDTL), " (", CMSTDYL, ")"),
      CMSTDTL != "" & CMSTDYL == "" ~
        paste0(toupper(CMSTDTL), ""),
      TRUE ~ ""
    ),
    ENDATE = case_when(
      CMENDTL != "" & CMENDYL != "" ~
        paste0(toupper(CMENDTL), " (", CMENDYL, ")"),
      CMENDTL != "" & CMENDYL == "" ~
        paste0(toupper(CMENDTL), ""),
      CMENDTL == "" & CMENRF == "AFTER" ~ "Ongoing",
      TRUE ~ ""
    ),
    DOSEU = case_when(
      (!is.na(CMDOSE)) ~ paste(CMDOSE, CMDOSU, " "),
      is.na(CMDOSE) & !is.na(CMDOSTXT) ~ paste(CMDOSTXT, CMDOSU, " "),
      is.na(CMDOSE) & is.na(CMDOSTXT) ~ ""
    ),
    COL0 = explicit_na(.data[[trtvar]], ""),
    COL1 = explicit_na(USUBJID, ""),
    # Optional Variable: CMDECOD/CMBASPRF
    COL2 = paste(
      stringr::str_to_sentence(CMTRT),
      stringr::str_to_sentence(CMDECOD),
      sep = concat_sep
    ),
    COL3 = explicit_na(POF, ""),
    # Optional Column: COL4/CQzzNAM
    COL4 = explicit_na(CQNAM, ""),
    # Optional Variable: CMLVL1/CMLVL2/CMLVL3/CMLVL4
    COL5 = paste(
      stringr::str_to_sentence(CMLVL1),
      stringr::str_to_sentence(CMLVL2),
      sep = concat_sep
    ),
    COL6 = explicit_na(STDATE, ""),
    COL7 = explicit_na(ENDATE, ""),
    COL8 = explicit_na(DOSEU, ""),
    COL9 = explicit_na(CMDOSFRQ, ""),
    # Optional Column: COL10/CMROUTE
    COL10 = explicit_na(stringr::str_to_sentence(CMROUTE), ""),
    COL11 = explicit_na(CMINDCSP, "")
  ) %>%
  arrange(COL0, COL1, ASTDT)

lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  # Optional Variable: CMDECOD/CMBASPRF
  # COL2 = paste("Medication (Verbatim)",
  #              "Standardized Medication Name",
  #              sep = concat_sep),
  COL2 = paste(
    "Medication (Verbatim)",
    "Base Preferred Term",
    sep = concat_sep
  ),
  COL3 = paste("Prior", "Concomitant", sep = concat_sep),
  # Optional Column: COL4/CQzzNAM
  COL4 = "Interest Category",
  # Optional Variable: CMLVL1/CMLVL2/CMLVL3/CMLVL4
  COL5 = paste("ATC Level 1", "ATC Level 2", sep = concat_sep),
  COL6 = "Start Date of Medication (Study Day~[super a])",
  COL7 = "End Date of Medication (Study Day~[super a])",
  COL8 = "Dose (Unit)",
  COL9 = "Frequency",
  # Optional Column: COL10/CMROUTE
  COL10 = "Route",
  COL11 = "Indication:Specify"
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
