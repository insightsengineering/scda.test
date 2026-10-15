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

tblid <- "LSIDS03"
fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01P"
key_cols <- c("COL0", "COL1")
sort_cols <- c("COL0", "COL1")
disp_cols <- paste0("COL", 0:9)
concat_sep <- " / "
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


###############################################################################
# Process data
###############################################################################

adsl <- adsl_jnj |>
  filter(!!rlang::sym(popfl) == "Y" & UNBLNDFL == "Y") |>
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
      case_when(
        SEX == "F" ~ "Female",
        SEX == "M" ~ "Male"
      ),
      levels = c("Female", "Male")
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

adexsum <- adexsum_jnj |>
  filter(PARAMCD == "CUMDOSE") |>
  select(STUDYID, USUBJID, PARAMCD, PARAM, AVAL)

adsl_adexsum <- left_join(
  adsl,
  adexsum,
  by = c(
    "STUDYID" = "STUDYID",
    "USUBJID" = "USUBJID"
  )
)

lsting <- adsl_adexsum |>
  mutate(
    AGE = explicit_na(as.character(AGE), ""),
    SEX = explicit_na(SEX, ""),
    RACE = explicit_na(RACE, ""),
    AVAL = explicit_na(as.character(AVAL), ""),
    AVALU = case_when(
      !is.na(AVAL) ~ stringr::str_extract(PARAM, "(?<=\\()([^()]*?)(?=\\)[^()]*$)"),
      is.na(AVAL) ~ ""
    ),
    EOTSTT = explicit_na(EOTSTT, ""),
    EOSSTT = explicit_na(EOSSTT, ""),
    COL0 = explicit_na(.data[[trtvar]], ""),
    COL1 = explicit_na(USUBJID, ""),
    COL2 = paste(AGE, SEX, RACE, sep = concat_sep),
    # Optional Column: COL3/LTVISIT
    COL3 = explicit_na(LTVISIT, ""),
    #COL4 = explicit_na(as.character(UNBLNDDY), ""),
    COL4 = ifelse(
      is.na(UNBLNDDT),
      "",
      toupper(format(as.Date(UNBLNDDT), format = "%d%b%Y"))
    ),
    #COL5 = explicit_na(as.character(TRTEDY), ""),
    COL5 = ifelse(
      is.na(TRTEDT),
      "",
      toupper(format(as.Date(TRTEDT), format = "%d%b%Y"))
    ),
    # Optional Column: COL6/CUMDOSE/CUMDOSU
    COL6 = paste0(AVAL, " ", AVALU),
    COL7 = explicit_na(stringi::stri_trans_totitle(UNBREAS), ""),
    COL8 = case_when(
      EOTSTT == "DISCONTINUED" ~ "Yes",
      EOTSTT != "DISCONTINUED" ~ "No"
    ),
    COL9 = case_when(
      EOSSTT == "DISCONTINUED" ~ "Yes",
      EOSSTT != "DISCONTINUED" ~ "No"
    )
  ) |>
  arrange(COL0, COL1)

lsting <- lsting |>
  mutate(
    COL4 = ifelse(is.na(UNBLNDDY), COL4, sprintf("%s (%s)", COL4, UNBLNDDY)),
    COL5 = ifelse(is.na(TRTEDY), COL5, sprintf("%s (%s)", COL5, TRTEDY))
  )

lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  COL2 = paste("Age (years)", "Sex", "Race", sep = concat_sep),
  # Optional Column: COL3/LTVISIT
  COL3 = "Last Visit~[super a]",
  COL4 = "Date of Unblinding (Study Day~[super b])",
  COL5 = "Date of Last Study Agent Administered (Study Day~[super b])",
  # Optional Column: COL6/CUMDOSE/CUMDOSU
  COL6 = "Cumulative Dose (unit)",
  COL7 = "Reason for Unblinding",
  COL8 = "Was Study Agent Discontinued?",
  COL9 = "Was Study Participation Discontinued Prematurely?"
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


colwidth <- c(21, 13, 67, 18, 23, 33, 20, 20, 25, 39)

tt_to_tlgrtf(head(result, 100), file = fileid, orientation = "landscape")
