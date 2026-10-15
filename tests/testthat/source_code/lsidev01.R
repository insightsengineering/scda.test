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

tblid <- "LSIDEV01"
fileid <- write_path(opath, tblid)
popfl <- "FASFL"
trtvar <- "TRT01A"
key_cols <- c("COL0", "COL1", "COL2", "COL3", "COL4", "COL5")
sort_cols <- c("COL0", "COL1", "COL2", "COL3", "COL4", "COL5")
disp_cols <- paste0("COL", 0:8)
concat_sep <- " / "
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


###############################################################################
# Process data
###############################################################################

adsl <- adsl_jnj |>
  filter(!!rlang::sym(popfl) == "Y") |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c("Xanomeline Low Dose", "Xanomeline High Dose", "Placebo")
    ),
    SEX = factor(
      case_when(SEX == "M" ~ "Male", SEX == "F" ~ 'Female', TRUE ~ SEX),
      levels = c("Male", "Female", "Intersex", "Unknown")
    ),
    RACE = factor(
      case_when(
        RACE == "AMERICAN INDIAN OR ALASKA NATIVE" ~ "American Indian or Alaska Native",
        RACE == "ASIAN" ~ "Asian",
        RACE == "BLACK OR AFRICAN AMERICAN" ~ "Black or African American",
        RACE == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ "Native Hawaiian or other Pacific Islander",
        RACE == "WHITE" ~ "White",
        RACE == "MULTIPLE" ~ "Multiple",
        RACE == "NOT REPORTED" ~ "Not reported",
        RACE == "UNKNOWN" ~ "Unknown",
        RACE == "OTHER" ~ "Other"
      ),
      levels = c(
        "American Indian or Alaska Native",
        "Asian",
        "Black or African American",
        "Native Hawaiian or other Pacific Islander",
        "White",
        "Multiple",
        "Not reported",
        "Unknown",
        "Other"
      )
    )
  )

dv <- dv_jnj |>
  filter(toupper(DVCAT) == "MAJOR") |>
  select(STUDYID, USUBJID, DVSEQ, DVDECOD, DVTERM, DVSTDTC, DVDECOD) |>
  mutate(
    DVSTDT = as.Date(DVSTDTC),
  )

lsting <- adsl |>
  dplyr::inner_join(dv, by = c("STUDYID", "USUBJID")) |>
  mutate(
    AGE = explicit_na(as.character(AGE), ""),
    SEX = explicit_na(SEX, ""),
    RACE = explicit_na(RACE, ""),
    DVDECOD = explicit_na(DVDECOD, ""),
    DVTERM = explicit_na(DVTERM, ""),
    COL0 = explicit_na(REGION1, ""),
    COL1 = explicit_na(SITEID, ""),
    COL2 = explicit_na(.data[[trtvar]], ""),
    COL3 = explicit_na(USUBJID, ""),
    COL4 = paste(AGE, SEX, RACE, sep = concat_sep),
    COL5 = explicit_na(as.character(DVSTDT)),
    COL6 = as.integer(DVSTDT - TRTSDT + (DVSTDT >= TRTSDT)),
    COL7 = explicit_na(DVDECOD),
    COL8 = explicit_na(DVTERM)
  ) |>
  arrange(COL0, COL1, COL2, COL3, COL4, COL5)


lsting <- var_relabel(
  lsting,
  COL0 = "Region",
  COL1 = "Site ID",
  COL2 = "Treatment Group",
  COL3 = "Subject ID",
  COL4 = paste("Age (years)", "Sex", "Race", sep = concat_sep),
  COL5 = "Start Date of Deviation",
  COL6 = "Study Day~[super a] of Start of Deviation",
  COL7 = "Protocol Deviation Category",
  COL8 = "Protocol Deviation Verbatim Term"
)

###############################################################################
# Build listing
###############################################################################

result <- rlistings::as_listing(
  df = lsting,
  key_cols = key_cols,
  disp_cols = disp_cols,
  sort_cols = sort_cols,
  round_type = "sas"
)

###############################################################################
# Add titles and footnotes
###############################################################################

result <- set_titles(result, tab_titles)

###############################################################################
# Output listing
###############################################################################


colwidth <- c(16, 8, 21, 13, 49, 17, 19, 71, 71)

tt_to_tlgrtf(head(result, 100), file = fileid, orientation = "landscape")
