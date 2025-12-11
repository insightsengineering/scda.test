###############################################################################
## Original Reporting Effort: Standards
## Program Name:              lsidem02.R
## R version:                 4.2.1
## Short Description:         Create LSIDEM02:  Randomization Listing
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      2024-01-17
## Input:                     ADSL
## Output:                    lsidem02.rtf
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

tblid <- "LSIDEM02"
fileid <- write_path(opath, tblid)
popfl <- "RANDFL"
key_cols <- c("COL1")
disp_cols <- paste0("COL", 1:9)
concat_sep <- " / "
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


###############################################################################
# Process data
###############################################################################

adsl <- adsl_jnj %>%
  filter(!!rlang::sym(popfl) == "Y")

lsting <- adsl %>%
  mutate(
    COL1 = explicit_na(USUBJID, ""),
    COL2 = explicit_na(REGION1, ""),
    COL3 = explicit_na(COUNTRY_DECODE, ""),
    COL4 = case_when(
      !is.na(RANDDT) & any(names(adsl) == "RANDDTM") & !is.na(RANDDTM) ~
        paste0(
          toupper(format(RANDDT, "%d%b%Y")),
          concat_sep,
          substr(RANDDTM, 12, 16)
        ),
      !is.na(RANDDT) & any(names(adsl) == "RANDDTM") & is.na(RANDDTM) ~
        paste0(toupper(format(RANDDT, "%d%b%Y")), concat_sep, "--:--"),
      !is.na(RANDDT) & !any(names(adsl) == "RANDDTM") ~
        paste0(toupper(format(RANDDT, "%d%b%Y"))),
    ),
    COL5 = explicit_na(RANUM, ""),
    # Optional Column: COL6/STRAT1R
    COL6 = explicit_na(STRAT1R, ""),
    # Optional Column: COL7/STRAT2R
    COL7 = explicit_na(STRAT2R, ""),
    COL8 = explicit_na(TRT01P, ""),
    COL9 = explicit_na(TRT01A, "")
  ) %>%
  arrange(COL1)

lsting <- var_relabel(
  lsting,
  COL1 = "Subject ID",
  COL2 = "Region",
  COL3 = paste("Country", "Territory", sep = concat_sep),
  COL4 = paste("Randomization Date", "Time", sep = concat_sep),
  COL5 = "Randomization Number",
  # Optional Column: COL6/STRAT1R
  COL6 = "Stratification Factor 1",
  # Optional Column: COL7/STRAT2R
  COL7 = "Stratification Factor 2",
  COL8 = "Randomization Treatment Assignment",
  COL9 = "Actual Treatment"
)

###############################################################################
# Build listing
###############################################################################

result <- rlistings::as_listing(
  df = lsting,
  key_cols = c("COL1"),
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
