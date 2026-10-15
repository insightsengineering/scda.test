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

tblid <- "LSIDEV03"
fileid <- write_path(opath, tblid)
popfl <- "FASFL"
trtvar <- "TRT01A"
key_cols <- c("COL0", "COL1", "COL2", "COL3")
sort_cols <- c("COL0", "COL1", "COL2", "COL3")
disp_cols <- paste0("COL", 0:6)
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

ie <- ie_jnj |>
  select(STUDYID, USUBJID, IETESTCD, IETEST)

lsting <- adsl |>
  dplyr::inner_join(ie, by = c("STUDYID", "USUBJID")) |>
  mutate(
    AGE = explicit_na(as.character(AGE), ""),
    SEX = explicit_na(SEX, ""),
    RACE = explicit_na(RACE, ""),
    IETESTCD = explicit_na(IETESTCD, ""),
    IETEST = explicit_na(IETEST, ""),
    COL0 = explicit_na(REGION1, ""),
    COL1 = explicit_na(SITEID, ""),
    COL2 = explicit_na(.data[[trtvar]], ""),
    COL3 = explicit_na(USUBJID, ""),
    COL4 = paste(AGE, SEX, RACE, sep = concat_sep),
    COL5 = explicit_na(IETESTCD),
    COL6 = explicit_na(IETEST)
  ) |>
  arrange(COL0, COL1, COL2, COL3)


lsting <- var_relabel(
  lsting,
  COL0 = "Region",
  COL1 = "Site ID",
  COL2 = "Treatment Group",
  COL3 = "Subject ID",
  COL4 = paste("Age (years)", "Sex", "Race", sep = concat_sep),
  COL5 = paste("Inclusion", "Exclusion Criterion Number", sep = concat_sep),
  COL6 = paste("Inclusion", "Exclusion Description", sep = concat_sep)
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


colwidth <- c(16, 8, 21, 63, 67, 29, 70)

tt_to_tlgrtf(head(result, 100), file = fileid, orientation = "landscape")
