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

tblid <- "LSIEX01"
fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01A"
key_cols <- c("COL0", "COL1", "COL2")
sort_cols <- c("COL0", "COL1", "COL2")
disp_cols <- paste0("COL", 0:11)
concat_sep <- " / "
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


###############################################################################
# Process data
###############################################################################

adex <- adex_jnj |>
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

lsting <- adex |>
  mutate(
    across(matches("^ADECOD\\d+$"), stringr::str_to_sentence)
  ) |>
  unite(
    "decod",
    matches("^ADECOD\\d+$"),
    sep = ", ",
    na.rm = TRUE,
    remove = FALSE
  ) |>
  mutate(
    decod = ifelse(decod == "", NA, decod),
    AGE = explicit_na(as.character(AGE), ""),
    SEX = explicit_na(SEX, ""),
    RACE = explicit_na(RACE, ""),
    ASTDT = ifelse(
      !is.na(ASTDT) & nchar(as.character(ASTDT)) == 10,
      toupper(format(ASTDT, "%d%b%Y")),
      ""
    ),
    ASTTM = ifelse(!is.na(ASTDTM), substr(as.character(ASTDTM), 12, 16), ""),
    ASTDYN = ifelse(!is.na(ASTDY), ASTDY, NA),
    ASTDY = ifelse(!is.na(ASTDY), ASTDY, ""),
    AENDT = ifelse(
      !is.na(AENDT) & nchar(as.character(AENDT)) == 10,
      toupper(format(AENDT, "%d%b%Y")),
      ""
    ),
    AENTM = ifelse(!is.na(AENDTM), substr(as.character(AENDTM), 12, 16), ""),
    AENDY = ifelse(!is.na(AENDY), AENDY, ""),
    PRDOSE = ifelse(is.na(ASCHDOSE), "", paste(ASCHDOSE, ASCHDOSU)),
    preas = case_when(
      toupper(AADJP) == "ADVERSE EVENT" ~ paste0(stringr::str_to_sentence(AADJP), " (AE: ", decod, ")"),
      toupper(AADJP) == "OTHER" ~ paste0(stringr::str_to_sentence(AADJP), ": ", stringr::str_to_sentence(AADJPOTH)),
      !is.na(AADJP) ~ stringr::str_to_sentence(AADJP),
      TRUE ~ ""
    ),
    dreas = case_when(
      toupper(AADJ) == "ADVERSE EVENT" ~ paste0(stringr::str_to_sentence(AADJ), " (AE: ", decod, ")"),
      toupper(AADJ) == "OTHER" ~ paste0(stringr::str_to_sentence(AADJ), ": ", stringr::str_to_sentence(AADJOTH)),
      !is.na(AADJ) ~ stringr::str_to_sentence(AADJ),
      TRUE ~ ""
    ),
    dly_reas = case_when(
      toupper(ARSDOSD) == "OTHER" ~ paste0(stringr::str_to_sentence(ARSDOSD), ": ", stringr::str_to_sentence(ARSDSDO)),
      !is.na(ARSDOSD) ~ stringr::str_to_sentence(ARSDOSD),
      TRUE ~ ""
    ),
    ACTDOSE = ifelse(is.na(ADOSE), "", paste(ADOSE, ADOSU)),
    ADOSFRQ = explicit_na(ADOSFRQ, ""),
    ADOSFRQP = explicit_na(ADOSFRQP, ""),
    COL0 = explicit_na(.data[[trtvar]], ""),
    COL1 = explicit_na(USUBJID, ""),
    COL2 = paste(AGE, SEX, RACE, sep = concat_sep),
    # Optional Column: COL3/ATPT
    COL3 = explicit_na(stringr::str_to_sentence(ATPT), ""),
    COL4 = paste(
      PRDOSE,
      ADOSFRQP,
      sep = concat_sep
    ),
    # Optional Column: COL5/ADOSFRM/AROUTE
    COL5 = paste(
      stringr::str_to_sentence(ADOSFRM),
      stringr::str_to_sentence(AROUTE),
      sep = concat_sep
    ),
    # Optional Column: COL6/AACTPR/AADJP/AADJPOTH/AEDECODy(if Adverse Event)
    COL6 = case_when(
      !is.na(AACTPR) & preas != "" ~ paste(stringr::str_to_sentence(AACTPR), preas, sep = concat_sep),
      !is.na(AACTPR) & preas == "" ~ paste(stringr::str_to_sentence(AACTPR), "", sep = concat_sep),
      TRUE ~ ""
    ),
    # Optional column: COL7/ADOSDLY/ARSDOSD/ARSDSDO (Add when it is collected on study)
    COL7 = case_when(
      !is.na(ADOSDLY) & dly_reas != "" ~ paste(stringr::str_to_sentence(ADOSDLY), dly_reas, sep = concat_sep),
      !is.na(ADOSDLY) & ADOSDLY == 'Y' & dly_reas == "" ~ paste(
        stringr::str_to_sentence(ADOSDLY),
        "",
        sep = concat_sep
      ),
      !is.na(ADOSDLY) & dly_reas == "" ~ stringr::str_to_sentence(ADOSDLY),
      TRUE ~ ""
    ),
    COL8 = case_when(
      !is.na(AACTDU) & dreas != "" ~ paste(stringr::str_to_sentence(AACTDU), dreas, sep = concat_sep),
      !is.na(AACTDU) & dreas == "" ~ paste(stringr::str_to_sentence(AACTDU), "", sep = concat_sep),
      TRUE ~ ""
    ),
    COL9 = paste(
      stringr::str_to_sentence(ACTDOSE),
      ADOSFRQ,
      sep = concat_sep
    ),
    COL10 = case_when(
      ASTDT == "" ~ "",
      ASTDT != "" & ASTTM != "" & ASTDY != "" ~ paste0(ASTDT, concat_sep, ASTTM, " (", ASTDY, ")"),
      ASTDT != "" & ASTTM == "" & ASTDY != "" ~ paste0(ASTDT, concat_sep, "--:--", " (", ASTDY, ")"),
      ASTDT != "" & ASTTM != "" & ASTDY == "" ~ paste0(ASTDT, concat_sep, ASTTM, " (-)"),
      ASTDT != "" & ASTTM == "" & ASTDY == "" ~ paste0(ASTDT, concat_sep, "--:--", " (-)"),
    ),
    # Optional column: COL11/AENDT/AENDY
    COL11 = case_when(
      AENDT == "" ~ "",
      AENDT != "" & AENDY != "" ~ paste0(AENDT, concat_sep, " (", AENDY, ")"),
      AENDT != "" & AENDY == "" ~ paste0(AENDT, concat_sep, " (-)"),
    )
  ) |>
  arrange(COL0, COL1, COL2, !is.na(ASTDYN), ASTDYN, ASTDTM, AVISITN)

lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  COL2 = paste("Age (years)", "Sex", "Race", sep = concat_sep),
  # Optional Column: COL3/ATPT
  COL3 = "Time Point",
  COL4 = paste("Prescribed Dose (unit)", "Frequency", sep = concat_sep),
  # Optional Column: COL5/ADOSFRM/AROUTE
  COL5 = paste("Formulation", "Route", sep = concat_sep),
  # Optional Column: COL6/AACTPR/AADJP/AADJPOTH/AEDECODy(if Adverse Event)
  COL6 = paste("Action Taken to Prescribed Dose~[super a]", "Reason", sep = concat_sep),
  # Optional column: COL7/ADOSDLY/ARSDOSD/ARSDSDO (Add when it is collected on study)
  COL7 = paste("Dose Delayed?", "Reason", sep = concat_sep),
  COL8 = paste("Action Taken With Study Treatment", "Reason", sep = concat_sep),
  COL9 = paste("Actual Dose (unit)", "Frequency", sep = concat_sep),
  COL10 = paste("Start Date", "Time (Study Day~[super a])", sep = concat_sep),
  # Optional column: COL11/AENDT/AENDY
  COL11 = paste("End Date", "(Study Day~[super a])", sep = concat_sep)
)

###############################################################################
# Build listing
###############################################################################

result <- rlistings::as_listing(
  df = lsting,
  key_cols = key_cols,
  sort_cols = sort_cols,
  disp_cols = disp_cols,
  round_type = "sas"
)

###############################################################################
# Add titles and footnotes
###############################################################################

result <- set_titles(result, tab_titles)

###############################################################################
# Output listing
###############################################################################


colwidth <- c(21, 13, 18, 15, 21, 23, 39, 32, 33, 20, 25, 25)

tt_to_tlgrtf(head(result, 100), file = fileid, orientation = "landscape")
