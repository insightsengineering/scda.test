library(envsetup)
library(tern)
library(dplyr)
library(tidyr)
library(rtables)
library(rlistings)
library(junco)
library(tidytlg)

###############################################################################
# Define script level parameters
###############################################################################

tblid <- "LSFECG01"
fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01A"
key_cols <- c("COL0", "COL1", "COL2", "COL3")
sort_cols <- c("COL0", "COL1", "COL2", "COL3")
disp_cols <- paste0("COL", 0:10)
concat_sep <- " / "
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

###############################################################################
# Process data
###############################################################################

adeg <- adeg_jnj |>
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
    PARAM = factor(
      .data$PARAM,
      levels = unique(.data[['PARAM']])[order(unique(.data[['PARAMN']]))]
    ),
    AVISIT = factor(
      .data[['AVISIT']],
      levels = unique(.data[['AVISIT']])[order(unique(.data[['AVISITN']]))]
    )
  )

adeg_crit <- adeg |>
  # Update CRITyFL variabes based on study requirements
  filter(CRIT1FL == "Y" | CRIT2FL == "Y") |>
  select(STUDYID, USUBJID, PARAMCD) |>
  distinct()

adeg_dig <- tidytlg:::make_precision_data(
  df = adeg,
  decimal = 4,
  precisionby = "PARAMCD",
  precisionon = "AVAL"
) |>
  rename(c(VALDIGMAX = "decimal"))

adeg_list <- adeg_crit |>
  inner_join(
    adeg,
    by = c(
      "STUDYID" = "STUDYID",
      "USUBJID" = "USUBJID",
      "PARAMCD" = "PARAMCD"
    )
  ) |>
  inner_join(adeg_dig, by = c("PARAMCD" = "PARAMCD"))

lsting <- adeg_list |>
  mutate(
    AGE = explicit_na(as.character(AGE), ""),
    SEX = explicit_na(SEX, ""),
    RACE = explicit_na(RACE, ""),
    ADT = ifelse(
      nchar(as.character(ADT)) == 10,
      toupper(format(ADT, "%d%b%Y")),
      "---------"
    ),
    ATM = ifelse(!is.na(ADTM), substr(as.character(ADTM), 12, 16), "--:--"),
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
    # Optional Column: CRITyFL
    CRIT1L = ifelse(CRIT1FL == "Y", as.character(CRIT1), NA_character_),
    CRIT2L = ifelse(CRIT2FL == "Y", as.character(CRIT2), NA_character_),
    TREM_FL = case_when(
      TRTEMFL == "Y" ~ "Yes",
      .default = NA
    )
  ) |>
  unite(
    "CRITL",
    CRIT1L,
    CRIT2L,
    sep = ", ",
    na.rm = TRUE,
    remove = FALSE
  ) |>
  mutate(
    COL0 = explicit_na(.data[[trtvar]], ""),
    COL1 = explicit_na(USUBJID, ""),
    COL2 = paste(AGE, SEX, RACE, sep = concat_sep),
    COL3 = explicit_na(PARAM, ""),
    # Optional Variable: ATM
    COL4 = paste(ADT, concat_sep, ATM, " (", ADY, ")", sep = ""),
    COL5 = explicit_na(AVISIT, ""),
    # Optional Column: COL6/ATPT
    COL6 = explicit_na(ATPT, ""),
    COL7 = explicit_na(VAL_RES, ""),
    # Optional Column: COL8/CHG
    COL8 = case_when(
      is.na(CHG) ~ "",
      is.na(VALDIGMAX) & !is.na(AVAL) & !is.na(CHG) ~
        tidytlg::roundSAS(CHG, digits = 0, as_char = TRUE, na_char = NULL),
      VALDIGMAX == 0 & !is.na(CHG) ~ tidytlg::roundSAS(CHG, digits = 0, as_char = TRUE, na_char = NULL),
      VALDIGMAX == 1 & !is.na(CHG) ~ tidytlg::roundSAS(CHG, digits = 1, as_char = TRUE, na_char = NULL),
      VALDIGMAX == 2 & !is.na(CHG) ~ tidytlg::roundSAS(CHG, digits = 2, as_char = TRUE, na_char = NULL),
      VALDIGMAX == 3 & !is.na(CHG) ~ tidytlg::roundSAS(CHG, digits = 3, as_char = TRUE, na_char = NULL),
      VALDIGMAX >= 4 & !is.na(CHG) ~ tidytlg::roundSAS(CHG, digits = 4, as_char = TRUE, na_char = NULL)
    ),
    # Optional Column: COL9/CRITy
    COL9 = explicit_na(CRITL, ""),
    COL10 = explicit_na(TREM_FL, "")
  ) |>
  arrange(COL0, COL1, COL2, COL3, !is.na(ADYN), ADYN, ADTM)

lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  COL2 = paste("Age (years)", "Sex", "Race", sep = concat_sep),
  COL3 = "ECG Parameter (unit)",
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
  # Optional Column: COL8/CHG
  COL8 = "Change From Baseline",
  # Optional Column: COL9/CRITy
  COL9 = "Criteria",
  COL10 = "Treatment-emergent?"
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


colwidth <- c(21, 13, 18, 32, 75, 16, 50, 12, 15, 14, 18)

tt_to_tlgrtf(head(result, 100), file = fileid, orientation = "landscape")
