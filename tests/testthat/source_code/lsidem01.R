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
sort_cols <- c("COL0", "COL1")
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

adsl <- adsl_jnj |>
  filter(.data[[popfl]] == "Y") |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c(
        "Xanomeline Low Dose",
        "Xanomeline High Dose",
        "Placebo"
      )
    ),
    SEX = factor(
      case_when(SEX == "M" ~ "Male", SEX == "F" ~ "Female", TRUE ~ SEX),
      levels = c("Male", "Female", "Intersex", "Unknown")
    ),
    COUNTRY = factor(
      case_when(
        COUNTRY == "USA" ~ "United States",
        TRUE ~ NA_character_
      )
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
    ),
    ETHNIC = factor(
      case_when(
        ETHNIC == "HISPANIC OR LATINO" ~ "Hispanic or Latino",
        ETHNIC == "NOT HISPANIC OR LATINO" ~ "Not Hispanic or Latino",
        ETHNIC == "NOT REPORTED" ~ "Not reported",
        ETHNIC == "UNKNOWN" ~ "Unknown"
      ),
      levels = c("Hispanic or Latino", "Not Hispanic or Latino", "Not reported", "Unknown")
    )
  )


lsting <- adsl |>
  mutate(
    AGE = explicit_na(as.character(AGE), ""),
    SEX = explicit_na(SEX, ""),
    RACE = explicit_na(RACE, ""),
    COL0 = explicit_na(.data[[trtvar]], ""),
    COL1 = explicit_na(USUBJID, ""),
    COL2 = explicit_na(REGION1, ""),
    COL3 = explicit_na(COUNTRY, ""),
    COL4 = explicit_na(toupper(format(RFICDT, "%d%b%Y")), ""),
    COL5 = paste(AGE, SEX, RACE, sep = concat_sep),
    COL6 = explicit_na(ETHNIC, ""),
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
  ) |>
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


colwidth <- c(21, 63, 16, 17, 23, 67, 17, 13, 12, 14, 12)

tt_to_tlgrtf(head(result, 100), file = fileid, orientation = "landscape")
