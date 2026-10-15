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

tblid <- "LSIEX03"
fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01A"
key_cols <- c("COL0", "COL1", "COL2")
sort_cols <- c("COL0", "COL1", "COL2")
disp_cols <- paste0("COL", 0:15)
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
    ),
    AVISIT = factor(
      .data[['AVISIT']],
      levels = unique(.data[['AVISIT']])[order(unique(.data[['AVISITN']]))]
    )
  )


lsting <- adex |>
  mutate(
    across(matches("^AACTDU\\d+$"), stringr::str_to_sentence),
    across(matches("^ADECOD\\d+$"), stringr::str_to_sentence)
  ) |>
  unite(
    "dact",
    matches("^AACTDU\\d+$"),
    sep = ", ",
    na.rm = TRUE,
    remove = FALSE
  ) |>
  unite(
    "decod",
    matches("^ADECOD\\d+$"),
    sep = ", ",
    na.rm = TRUE,
    remove = FALSE
  ) |>
  mutate(
    dact = ifelse(dact == "", NA, dact),
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
    AENDYN = ifelse(!is.na(AENDY), AENDY, NA),
    AENDY = ifelse(!is.na(AENDY), AENDY, ""),
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
      !is.na(ARSDOSD) ~ stringr::str_to_sentence(ARSDOSD),
      TRUE ~ ""
    ),
    COL0 = explicit_na(.data[[trtvar]], ""),
    COL1 = explicit_na(USUBJID, ""),
    COL2 = paste(AGE, SEX, RACE, sep = concat_sep),
    # Optional Column: COL3/AVISIT
    COL3 = explicit_na(AVISIT, ""),
    COL4 = ifelse(is.na(ASCHDOSE), "", paste(ASCHDOSE, ASCHDOSU)),
    COL5 = case_when(
      !is.na(AACTPR) & preas != "" ~ paste(stringr::str_to_sentence(AACTPR), preas, sep = concat_sep),
      !is.na(AACTPR) & preas == "" ~ paste(stringr::str_to_sentence(AACTPR), "", sep = concat_sep),
      TRUE ~ ""
    ),
    # Optional Column: COL6/ADOSFRQP
    COL6 = explicit_na(ADOSFRQP, ""),
    # Optional Column: COL7/ADOSDLY/ARSDOSD
    COL7 = case_when(
      !is.na(ADOSDLY) & dly_reas != "" ~ paste(stringr::str_to_sentence(ADOSDLY), dly_reas, sep = concat_sep),
      !is.na(ADOSDLY) & toupper(ADOSDLY) == 'Y' & dly_reas == "" ~ paste(
        stringr::str_to_sentence(ADOSDLY),
        "",
        sep = concat_sep
      ),
      !is.na(ADOSDLY) & dly_reas == "" ~ stringr::str_to_sentence(ADOSDLY),
      TRUE ~ ""
    ),
    # Optional Column: COL8/ATDPRP/ATDPRPU
    COL8 = ifelse(is.na(ATDPRP), "", paste(ATDPRP, ATDPRPU)),
    COL9 = case_when(
      !is.na(dact) & dreas != "" ~ paste(dact, dreas, sep = concat_sep),
      !is.na(dact) & dreas == "" ~ paste(stringr::str_to_sentence(dact), "", sep = concat_sep),
      TRUE ~ ""
    ),
    COL10 = ifelse(is.na(ACDOSE), "", ACDOSE),
    # Optional Column: COL11/AINFRAT/AINFRAU
    COL11 = ifelse(is.na(AINFRAT), "", paste(AINFRAT, AINFRAU)),
    COL12 = ifelse(is.na(ADOSE), "", paste(ADOSE, ADOSU)),
    COL13 = case_when(
      ASTDT == "" ~ "",
      ASTDT != "" & ASTTM != "" & ASTDY != "" ~ paste0(ASTDT, concat_sep, ASTTM, " (", ASTDY, ")"),
      ASTDT != "" & ASTTM == "" & ASTDY != "" ~ paste0(ASTDT, concat_sep, "--:--", " (", ASTDY, ")"),
      ASTDT != "" & ASTTM != "" & ASTDY == "" ~ paste0(ASTDT, concat_sep, ASTTM, " (-)"),
      ASTDT != "" & ASTTM == "" & ASTDY == "" ~ paste0(ASTDT, concat_sep, "--:--", " (-)"),
    ),
    COL14 = case_when(
      AENDT == "" ~ "",
      AENDT != "" & AENTM != "" & AENDY != "" ~ paste0(AENDT, concat_sep, AENTM, " (", AENDY, ")"),
      AENDT != "" & AENTM == "" & AENDY != "" ~ paste0(AENDT, concat_sep, "--:--", " (", AENDY, ")"),
      AENDT != "" & AENTM != "" & AENDY == "" ~ paste0(AENDT, concat_sep, AENTM, " (-)"),
      AENDT != "" & AENTM == "" & AENDY == "" ~ paste0(AENDT, concat_sep, "--:--", " (-)"),
    ),
    # Optional Column: COL15/ADURC
    COL15 = explicit_na(ADURC, "")
  ) |>
  arrange(
    COL0,
    COL1,
    COL2,
    !is.na(ASTDYN),
    ASTDYN,
    ASTDTM,
    !is.na(AENDYN),
    AENDYN,
    AENDTM
  )

lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  COL2 = paste("Age (years)", "Sex", "Race", sep = concat_sep),
  # Optional Column: COL3/AVISIT
  COL3 = "Visit",
  COL4 = "Prescribed Dose Level (unit)",
  COL5 = paste("Action Taken~[super a]", "Reason", sep = concat_sep),
  # Optional Column: COL6/ADOSFRQP
  COL6 = "Schedule Change New Dosing Frequency",
  # Optional Column: COL7/ADOSDLY/ARSDOSD
  COL7 = paste("Dose Delayed?", "Reason", sep = concat_sep),
  # Optional Column: COL8/ATDPRP/ATDPRPU
  COL8 = "Total Dose to be Administered (unit)",
  COL9 = paste("Action Taken", "Reason", sep = concat_sep),
  COL10 = "Total Volume of Investigational Product Infused (mL)",
  # Optional Column: COL11/AINFRAT/AINFRAU
  COL11 = "Infusion Rate (unit)",
  COL12 = "Actual Dose Administered (unit)",
  COL13 = paste("Start Date", "Time (Study Day~[super a])", sep = concat_sep),
  COL14 = paste("End Date", "Time (Study Day~[super a])", sep = concat_sep),
  # Optional Column: COL15/ADURC
  COL15 = "Duration (hh:mm)"
)

###############################################################################
# Build listing
###############################################################################

spanning_headers <- data.frame(
  span_level = c(1L, 1L),
  label = c("Prior to Infusion Start", "During Infusion"),
  start = c(5L, 9L),
  span = c(2L, 1L)
)

result <- rlistings::as_listing(
  df = lsting,
  key_cols = key_cols,
  sort_cols = sort_cols,
  disp_cols = disp_cols,
  spanning_col_labels = spanning_headers,
  round_type = "sas"
)

###############################################################################
# Add titles and footnotes
###############################################################################

result <- set_titles(result, tab_titles)

###############################################################################
# Output listing
###############################################################################


colwidth <- c(21, 13, 17, 17, 18, 32, 18, 32, 23, 33, 25, 15, 23, 25, 25, 16)

tt_to_tlgrtf( 
  result,
  file = fileid,
  orientation = "landscape",
  border_fns = list(
    tidytlg::no_borders,
    tidytlg::spanning_borders(1),
    tidytlg::row_border(2)
  )
)
