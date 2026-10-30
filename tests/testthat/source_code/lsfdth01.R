library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(rlistings)
library(junco)

###############################################################################
# Define script level parameters
###############################################################################

tblid <- "LSFDTH01"
fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01A"
paramcd_dur <- "TRTDURM"
key_cols <- c("COL0", "COL1", "COL2")
sort_cols <- c("COL0", "COL1", "COL2")
disp_cols <- paste0("COL", 0:8)
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
      levels = c("Male", "Female")
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
  ) |>
  filter(!!rlang::sym(popfl) == "Y" & DTHFL == "Y")

adexsum <- adexsum_jnj |>
  filter(PARAMCD == paramcd_dur) |>
  select(STUDYID, USUBJID, AVAL, PARAM)

adexsum_dig <- tidytlg:::make_precision_data(
  df = adexsum,
  decimal = 4,
  precisionby = "STUDYID",
  precisionon = "AVAL"
) |>
  rename(c(VALDIGMAX = "decimal"))

adsl_adexsum <- adsl |>
  inner_join(adexsum, by = c("STUDYID", "USUBJID")) |>
  left_join(adexsum_dig, by = c("STUDYID" = "STUDYID"))

lsting <- adsl_adexsum |>
  mutate(
    AGE = explicit_na(as.character(AGE), ""),
    SEX = explicit_na(SEX, ""),
    RACE = explicit_na(RACE, ""),
    LDOSE = explicit_na(as.character(LDOSE), ""),
    LDOSU = explicit_na(LDOSU, ""),
    DTHCAUS = explicit_na(stringr::str_to_sentence(DTHCAUS), ""),
    DTHTERM = explicit_na(stringr::str_to_sentence(DTHTERM), ""),
    COL0 = explicit_na(.data[[trtvar]], ""),
    COL1 = explicit_na(USUBJID, ""),
    COL2 = paste(AGE, SEX, RACE, sep = concat_sep),
    COL3 = paste0(LDOSE, " (", LDOSU, ")"),
    COL4 = case_when(
      is.na(AVAL) ~ "",
      VALDIGMAX == 0 & !is.na(AVAL) ~ tidytlg::roundSAS(AVAL, digits = 0, as_char = TRUE, na_char = NULL),
      VALDIGMAX >= 1 & !is.na(AVAL) ~ tidytlg::roundSAS(AVAL, digits = 1, as_char = TRUE, na_char = NULL)
    ),
    # Optional Column: COL5/TRTEDT/TRTEDY
    COL5 = ifelse(
      is.na(TRTEDT),
      "",
      paste0(
        toupper(format(TRTEDT, "%d%b%Y")),
        " (",
        TRTEDY,
        ")"
      )
    ),
    # Optional Column: COL6/LDSTODTH
    COL6 = explicit_na(as.character(LDSTODTH), ""),
    COL7 = ifelse(
      is.na(DTHDT),
      "",
      paste0(
        toupper(format(DTHDT, "%d%b%Y")),
        " (",
        DTHDY,
        ")"
      )
    ),
    COL8 = paste(DTHCAUS, DTHTERM, sep = concat_sep)
  ) |>
  arrange(COL0, COL1)

lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  COL2 = paste("Age (years)", "Sex", "Race", sep = concat_sep),
  # Optional Column: COL3/LDOSE/LDOSU
  COL3 = "Dose (Unit) of Last Dose of Study Treatment",
  COL4 = paste0(
    "Duration of Treatment ",
    tolower(trimws(gsub(
      "[()]",
      "",
      sub("(?i)^duration of treatment[, ]*", "", adsl_adexsum[["PARAM"]][1], perl = TRUE)
    ))),
    "~[super a]"
  ),

  # Optional Column: COL5/TRTEDT/TRTEDY
  COL5 = "Date of Last Dose of Study Treatment (Study Day~[super b])",
  # Optional Column: COL6/LDSTODTH
  COL6 = "Days From Last Study Treatment Administration to Death~[super c]",
  COL7 = "Date of Death (Study Day~[super d])",
  COL8 = paste(
    "Primary Cause of Death (Preferred Term",
    "Verbatim)",
    sep = concat_sep
  )
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


colwidth <- c(21, 63, 33, 27, 19, 31, 41, 22, 28)

tt_to_tlgrtf(head(result, 100), file = fileid, orientation = "landscape")
