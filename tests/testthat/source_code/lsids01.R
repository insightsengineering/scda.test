library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(rlistings)
library(junco)

###############################################################################
# Define script level parameters
###############################################################################

tblid <- "LSIDS01"
fileid <- write_path(opath, tblid)
popfl <- "FASFL"
trtvar <- "TRT01P"
key_cols <- c("COL0", "COL1")
sort_cols <- c("COL0", "COL1", "COL2", "COL3")
disp_cols <- paste0("COL", 0:8)
concat_sep <- " / "
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


###############################################################################
# Process data
###############################################################################

adsl <- adsl_jnj |>
  filter(!!rlang::sym(popfl) == "Y" & !(EOTSTT %in% c("COMPLETED", "ONGOING"))) |>
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

ds <- ds_jnj |>
  filter(
    (DSSCAT %in% c("TREATMENT")) &
      DSCAT == "DISPOSITION EVENT" &
      DSDECOD != "COMPLETED"
  ) |>
  select(STUDYID, USUBJID, DSSCAT)

adsl_ds <- adsl |>
  left_join(ds, by = c("STUDYID" = "STUDYID", "USUBJID" = "USUBJID"))

adexsum <- adexsum_jnj |>
  filter(PARAMCD == "CUMDOSE") |>
  select(STUDYID, USUBJID, PARAMCD, PARAM, AVAL)

adsl_ds_adexsum <- left_join(
  adsl_ds,
  adexsum,
  by = c(
    "STUDYID" = "STUDYID",
    "USUBJID" = "USUBJID"
  )
)

lsting <- adsl_ds_adexsum |>
  mutate(
    AGE = explicit_na(as.character(AGE), ""),
    SEX = explicit_na(SEX, ""),
    RACE = explicit_na(RACE, ""),
    AVAL = explicit_na(as.character(AVAL), ""),
    AVALU = case_when(
      !is.na(AVAL) ~ stringr::str_extract(PARAM, "(?<=\\()([^()]*?)(?=\\)[^()]*$)"),
      is.na(AVAL) ~ ""
    ),
    DCTREAS = explicit_na(DCTREAS, ""),
    DCTREASP = explicit_na(DCTREASP, ""),
    COL0 = explicit_na(.data[[trtvar]], ""),
    COL1 = explicit_na(USUBJID, ""),
    COL2 = paste(AGE, SEX, RACE, sep = concat_sep),
    # Optional Column: COL3/DSSCAT
    COL3 = explicit_na(stringr::str_to_sentence(DSSCAT), ""),
    # Optional Column: COL4/LTVISIT
    COL4 = explicit_na(LTVISIT, ""),
    COL5 = ifelse(
      is.na(TRTEDT),
      "",
      toupper(format(as.Date(TRTEDT), format = "%d%b%Y"))
    ),
    # Optional Column: COL6/CUMDOSE/CUMDOSU
    COL6 = paste0(AVAL, " ", AVALU),
    COL7 = ifelse(
      is.na(DCTDT),
      "",
      toupper(format(as.Date(DCTDT), format = "%d%b%Y"))
    ),
    COL8 = case_when(
      DCTREAS == "OTHER" ~ paste0(DCTREAS, " (", stringr::str_to_sentence(DCTREASP), ")"),
      DCTREAS != "OTHER" ~ DCTREAS
    )
  ) |>
  arrange(COL0, COL1, COL2, COL3)

lsting <- lsting |>
  mutate(
    COL7 = ifelse(is.na(DCTADY), COL7, sprintf("%s (%s)", COL7, DCTADY)),
    COL5 = ifelse(is.na(TRTEDY), COL7, sprintf("%s (%s)", COL5, TRTEDY))
  )

lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  COL2 = paste("Age (years)", "Sex", "Race", sep = concat_sep),
  # Optional Column: COL3/DSSCAT
  COL3 = "Study Agent Discontinued",
  # Optional Column: COL4/LTVISIT
  COL4 = "Last Visit~[super a]",
  COL5 = "Date of Last Study Agent Administered (Study Day~[super b])",
  # Optional Column: COL6/CUMDOSE/CUMDOSU
  COL6 = "Cumulative Dose (unit)",
  COL7 = "Date of Discontinuation (Study Day~[super b])",
  COL8 = "Primary Reason for Discontinuation"
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


colwidth <- c(21, 25, 67, 23, 36, 33, 20, 33, 27)

tt_to_tlgrtf(head(result, 100), file = fileid, orientation = "landscape")
