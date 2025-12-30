###############################################################################
## Original Reporting Effort: Standards
## Program Name:              lsidem01.R
## R version:                 4.2.1
## Short Description:         Create LSIDEM01: Listing of Demographics and
##                            Baseline Characteristics
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      2024-01-17
## Input:                     ADSL
## Output:                    lsidem01.rtf
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

tblid <- "LSIDEM01"
fileid <- write_path(opath, tblid)
popfl <- "FASFL"
trtvar <- "TRT01P"
key_cols <- c("COL0", "COL1")
disp_cols <- paste0("COL", 0:10)
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
  filter(!!rlang::sym(popfl) == "Y")

lsting <- adsl %>%
  mutate(
    AGE = explicit_na(as.character(AGE), ""),
    SEX = explicit_na(SEX, ""),
    RACE_DECODE = explicit_na(RACE_DECODE, ""),
    COL0 = explicit_na(.data[[trtvar]], ""),
    COL1 = explicit_na(USUBJID, ""),
    COL2 = explicit_na(REGION1, ""),
    COL3 = explicit_na(COUNTRY_DECODE, ""),
    COL4 = explicit_na(toupper(format(RFICDT, "%d%b%Y")), ""),
    COL5 = paste(AGE, SEX, RACE_DECODE, sep = concat_sep),
    COL6 = explicit_na(ETHNIC_DECODE, ""),
    COL7 = explicit_na(
      tidytlg::roundSAS(WEIGHTBL, digits = 1, as_char = TRUE, na_char = ""),
      ""
    ),
    COL8 = explicit_na(
      tidytlg::roundSAS(HEIGHTBL, digits = 1, as_char = TRUE, na_char = ""),
      ""
    ),
    COL9 = explicit_na(
      tidytlg::roundSAS(BMIBL, digits = 2, as_char = TRUE, na_char = ""),
      ""
    ),
    # Optional Column: COL10/BSABL
    COL10 = explicit_na(
      tidytlg::roundSAS(BSABL, digits = 2, as_char = TRUE, na_char = ""),
      ""
    ),
  ) %>%
  arrange(COL0, COL1)

lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  COL2 = "Region",
  COL3 = paste("Country", "Territory", sep = concat_sep),
  COL4 = "Informed Consent Date",
  COL5 = paste("Age (years)", "Sex", "Race", sep = concat_sep),
  COL6 = "Ethnicity",
  COL7 = "Weight (kg)",
  COL8 = "Height (cm)",
  COL9 = "BMI (kg/m~[super 2])",
  # Optional Column: COL10/BSABL
  COL10 = "BSA (m~[super 2])"
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
