library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(rlistings)
library(junco)

###############################################################################
# Define script level parameters
###############################################################################

tblid <- "LSFLAB01"
fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01A"
key_cols <- c("COL0", "COL1", "COL2", "COL3")
sort_cols <- c("COL0", "COL1", "COL2", "COL3")
disp_cols <- paste0("COL", 0:11)
concat_sep <- " / "
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

# Parameter to control Criteria Flag column: "CRIT" (default MCRIT1FL & MCRIT2FL),
# -- "ATOXGR" if user needs ATOXGR to be displayed
crit_var <- "CRIT"

###############################################################################
# Process data
###############################################################################

adlb <- adlb_jnj |>
  filter(!!rlang::sym(popfl) == "Y") |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c("Xanomeline Low Dose", "Xanomeline High Dose", "Placebo")
    ),
    SEX = factor(
      case_when(SEX == "M" ~ "Male", SEX == "F" ~ "Female", TRUE ~ SEX),
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
    PARAM := ordered(PARAM, levels = {
      # Sort by PARCAT1, PARCAT3N then PARAM and PARCAT3 not displayed used only for sorting
      unique(PARAM[order(PARCAT1, PARCAT3N, PARAM)])
    }),
    AVISIT = factor(
      .data[["AVISIT"]],
      levels = unique(.data[["AVISIT"]])[order(unique(.data[["AVISITN"]]))]
    )
  )

adlb_crit <- adlb |>
  # Update filter criteria as needed for your study
  # MCRIT1ML/MCRII2ML or ATOXGRN will be used
  filter(
    ((!is.na(MCRIT1ML) & as.numeric(substr(MCRIT1ML, 7, 8)) >= 2) |
      (!is.na(MCRIT2ML) & as.numeric(substr(MCRIT2ML, 7, 8)) >= 2))
  ) |>
  # filter(ATOXGRN >= 3) |>
  select(STUDYID, USUBJID, PARAMCD) |>
  distinct()

adlb_dig <- tidytlg:::make_precision_data(
  df = adlb,
  decimal = 4,
  precisionby = "PARAMCD",
  precisionon = "AVAL"
) |>
  rename(c(VALDIGMAX = "decimal"))

adlb_list <- adlb_crit |>
  inner_join(
    adlb,
    by = c(
      "STUDYID" = "STUDYID",
      "USUBJID" = "USUBJID",
      "PARAMCD" = "PARAMCD"
    )
  ) |>
  inner_join(adlb_dig, by = c("PARAMCD" = "PARAMCD"))

lsting <- adlb_list |>
  mutate(
    AGE = explicit_na(as.character(AGE), ""),
    SEX = explicit_na(SEX, ""),
    RACE = explicit_na(RACE, ""),
    ADTS = ADT,
    ADT = ifelse(
      nchar(as.character(ADT)) == 10,
      toupper(format(ADT, "%d%b%Y")),
      ""
    ),
    ATM = ifelse(!is.na(ADTM), substr(as.character(ADTM), 12, 16), ""),
    ADYN = ifelse(!is.na(ADY), ADY, NA),
    ADY = ifelse(!is.na(ADY), ADY, "--"),
    VAL_RES = case_when(
      is.na(VALDIGMAX) & !is.na(AVAL) ~ tidytlg::roundSAS(AVAL, digits = 0, as_char = TRUE, na_char = NULL),
      VALDIGMAX == 0 & !is.na(AVAL) ~ tidytlg::roundSAS(AVAL, digits = 0, as_char = TRUE, na_char = NULL),
      VALDIGMAX == 1 & !is.na(AVAL) ~ tidytlg::roundSAS(AVAL, digits = 1, as_char = TRUE, na_char = NULL),
      VALDIGMAX == 2 & !is.na(AVAL) ~ tidytlg::roundSAS(AVAL, digits = 2, as_char = TRUE, na_char = NULL),
      VALDIGMAX == 3 & !is.na(AVAL) ~ tidytlg::roundSAS(AVAL, digits = 3, as_char = TRUE, na_char = NULL),
      VALDIGMAX >= 4 & !is.na(AVAL) ~ tidytlg::roundSAS(AVAL, digits = 4, as_char = TRUE, na_char = NULL),
      !is.na(AVALC) ~ AVALC
    ),
    VAL_HL = case_when(
      !is.na(ANRIND) ~ substr(ANRIND, 1, 1),
      .default = NA
    ),
    VAL_CS = case_when(
      LBCLSIG == "Y" ~ "CS",
      LBCLSIG == "N" ~ "NCS",
      .default = NA
    ),
    VAL = case_when(
      !is.na(VAL_RES) & !is.na(VAL_HL) & !is.na(VAL_CS) ~
        paste(
          VAL_RES,
          VAL_HL,
          VAL_CS,
          sep = " "
        ),
      !is.na(VAL_RES) & !is.na(VAL_HL) & is.na(VAL_CS) ~
        paste(
          VAL_RES,
          VAL_HL,
          sep = " "
        ),
      !is.na(VAL_RES) & is.na(VAL_HL) & !is.na(VAL_CS) ~
        paste(
          VAL_RES,
          VAL_CS,
          sep = " "
        ),
      !is.na(VAL_RES) & is.na(VAL_HL) & is.na(VAL_CS) ~ VAL_RES,
      .default = NA
    ),
    ANRIND = substr(ANRIND, 1, 1),
    MCRIT1ML = ifelse(
      MCRIT1ML != "Level 0",
      paste0(
        "L",
        substr(MCRIT1ML, 7, 8),
        substr(
          MCRIT1ML,
          9,
          nchar(as.character(MCRIT1ML))
        )
      ),
      NA
    ),
    MCRIT2ML = ifelse(
      MCRIT2ML != "Level 0",
      paste0(
        "L",
        substr(MCRIT2ML, 7, 8),
        substr(
          MCRIT2ML,
          9,
          nchar(as.character(MCRIT2ML))
        )
      ),
      NA
    ),
    CRIT = case_when(
      is.na(MCRIT1ML) & is.na(MCRIT2ML) ~ "",
      !is.na(MCRIT1ML) ~ MCRIT1ML,
      !is.na(MCRIT2ML) ~ MCRIT2ML,
    ),
    # Optional column based on title
    ATOXGR = explicit_na(ATOXGR, ""),
    # CRIT = ifelse(!is.na(ATOXGR), as.character(ATOXGR), ""),
    REFRL = case_when(
      is.na(LBSTNRLO) ~ as.character(ANRLO),
      !is.na(LBSTNRLQ) & !is.na(LBSTNRLO) ~
        paste0(
          LBSTNRLQ,
          as.character(LBSTNRLO)
        ),
      is.na(LBSTNRLQ) & !is.na(LBSTNRLO) ~ as.character(LBSTNRLO),
      .default = "NA"
    ),
    REFRH = case_when(
      is.na(LBSTNRHI) ~ as.character(ANRHI),
      !is.na(LBSTNRHQ) & !is.na(LBSTNRHI) ~
        paste0(
          LBSTNRHQ,
          as.character(LBSTNRHI)
        ),
      is.na(LBSTNRHQ) & !is.na(LBSTNRHI) ~ as.character(LBSTNRHI),
      .default = "NA"
    ),
    REFR = case_when(
      !is.na(REFRL) & !is.na(REFRH) ~ paste(REFRL, REFRH, sep = " - "),
      !is.na(REFRL) & is.na(REFRH) ~ paste(REFRL, " ", sep = "- "),
      is.na(REFRL) & !is.na(REFRH) ~ paste(" ", REFRH, sep = "- ")
    ),
    TRTEM_FL = case_when(
      TRTEMFL == "Y" ~ "Yes",
      TRTEMFL == "N" ~ "No",
      .default = NA
    ),
    COL0 = explicit_na(.data[[trtvar]], ""),
    COL1 = explicit_na(USUBJID, ""),
    COL2 = paste(AGE, SEX, RACE, sep = concat_sep),
    COL3 = explicit_na(PARAM, ""),
    # Optional Variable: ATM
    COL4 = case_when(
      ADT == "" ~ "",
      ADT != "" & ATM != "" & ADY != "" ~ paste0(ADT, concat_sep, ATM, " (", ADY, ")"),
      ADT != "" & ATM == "" & ADY != "" ~ paste0(ADT, concat_sep, "--:--", " (", ADY, ")"),
      ADT != "" & ATM != "" & ADY == "" ~ paste0(ADT, concat_sep, ATM, " (-)"),
      ADT != "" & ATM == "" & ADY == "" ~ paste0(ADT, concat_sep, "--:--", " (-)"),
    ),
    COL5 = explicit_na(AVISIT, ""),
    # Optional Column: COL6/ATPT
    COL6 = explicit_na(ATPT, ""),
    COL7 = explicit_na(VAL, ""),
    # Optional Variable: COL8/(MCRIT1ML/MCRIT2ML)/ATOXGR
    COL8 = explicit_na(.data[[crit_var]], ""),
    COL9 = case_when(
      is.na(CHG) ~ "",
      is.na(VALDIGMAX) & !is.na(AVAL) & !is.na(CHG) ~
        tidytlg::roundSAS(CHG, digits = 0, as_char = TRUE, na_char = NULL),
      VALDIGMAX == 0 & !is.na(CHG) ~ tidytlg::roundSAS(CHG, digits = 0, as_char = TRUE, na_char = NULL),
      VALDIGMAX == 1 & !is.na(CHG) ~ tidytlg::roundSAS(CHG, digits = 1, as_char = TRUE, na_char = NULL),
      VALDIGMAX == 2 & !is.na(CHG) ~ tidytlg::roundSAS(CHG, digits = 2, as_char = TRUE, na_char = NULL),
      VALDIGMAX == 3 & !is.na(CHG) ~ tidytlg::roundSAS(CHG, digits = 3, as_char = TRUE, na_char = NULL),
      VALDIGMAX >= 4 & !is.na(CHG) ~ tidytlg::roundSAS(CHG, digits = 4, as_char = TRUE, na_char = NULL)
    ),
    # Optional Column: COL10/LBSTNRLO/LBSTNRHI/LBSTNRLQ/LBSTNRHQ/ANRLO/ANRHI
    COL10 = explicit_na(REFR, ""),
    # Optional Column: COL11/TRTEMFL
    COL11 = explicit_na(TRTEM_FL, "")
  ) |>
  arrange(COL0, COL1, COL2, COL3, ADTS, !is.na(ADYN), ADYN, ADTM)

lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  COL2 = paste("Age (years)", "Sex", "Race", sep = concat_sep),
  COL3 = "Laboratory Test (unit)",
  # Optional Variable: ATM
  COL4 = paste(
    "Assessment Date",
    "Time (Study Day~[super a])",
    sep = concat_sep
  ),
  COL5 = "Visit",
  # Optional Column: COL6/ATPT
  COL6 = "Time Point",
  COL7 = "Result",
  # Optional Variable: COL8/(MCRIT1ML/MCRIT2ML)/ATOXGR
  COL8 = "Criteria Flag",
  COL9 = "Change From Baseline",
  # Optional Column: COL10/LBSTNRLO/LBSTNRHI/LBSTNRLQ/LBSTNRHQ/ANRLO/ANRHI
  COL10 = "Reference Range",
  # Optional Column: COL11/TRTEMFL
  COL11 = "Treatment-emergent?"
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

colwidth <- c(21, 13, 18, 30, 75, 15, 25, 15, 22, 15, 18, 18)

tt_to_tlgrtf(
  head(result, 100),
  file = fileid,
  orientation = "landscape"
)
