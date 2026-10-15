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

tblid <- "LSFAE03"
fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01A"
key_cols <- c("COL0", "COL1", "COL2")
sort_cols <- c("COL0", "COL1", "COL2")
disp_cols <- paste0("COL", 0:10)
concat_sep <- " / "
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")
# Parameter to control whether time should be displayed
include_time <- TRUE
# Parameter to control whether imputed date to be displayed
include_imputed_dates <- FALSE

#Combination treatment parameters

# Provide whether this is a combination treatments
combination_trt <- FALSE

if (combination_trt) {
  comb_trtvars <- c("AEDRGS1", "AEDRGS2") # Provide the variables containing combination treatment information in sequence

  n_comb_trt <- length(comb_trtvars)

  comb_relvars <- paste0("AERELS", seq_len(n_comb_trt)) # This will create a variable list like AERELS1, AERELS2 etc
  comb_dosvars <- paste0("DOSS", seq_len(n_comb_trt), "ON") # This will create a variable list like DOSS1ON, DOSS2ON etc
  comb_dosuvars <- paste0("DOSS", seq_len(n_comb_trt), "U") # This will create a variable list like DOSS1U, DOSS2U etc
  comb_dosdyvars <- paste0("DOSS", seq_len(n_comb_trt), "DY") # This will create a variable list like DOSS1DY, DOSS2DY etc
}

###############################################################################
# Process data
###############################################################################

adae <- adae_jnj |>
  mutate(
    AEDECOD = case_when(
      AEDECOD == "" ~ paste0("Uncoded: ", AETERM),
      .default = AEDECOD
    )
  ) |>
  filter(!!rlang::sym(popfl) == "Y" & TRTEMFL == "Y" & TRDISCFL == "Y") |>
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
      dplyr::case_when(
        SEX == "F" ~ "Female",
        SEX == "M" ~ "Male",
        SEX == "U" ~ "Unknown",
        SEX == "INTERSEX" ~ "Intersex"
      ),
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
    AEOUT = factor(
      case_when(
        AEOUT == "FATAL" ~ "Fatal",
        AEOUT == "NOT RECOVERED/NOT RESOLVED" ~ "Not Recovered/Not Resolved",
        AEOUT == "RECOVERED/RESOLVED" ~ "Recovered/Resolved"
      ),
      levels = c(
        "Fatal",
        "Not Recovered/Not Resolved",
        "Recovered/Resolved"
      )
    ),
    AESER = factor(
      case_when(
        AESER == "Y" ~ "Yes",
        AESER == "N" ~ "No"
      ),
      levels = c(
        "Yes",
        "No"
      )
    ),
    AEREL = factor(
      case_when(
        AEREL == "NOT RELATED" ~ "Not Related",
        AEREL == "RELATED" ~ "Related"
      ),
      levels = c(
        "Not Related",
        "Related"
      )
    )
  )

###################################
# Section for combination treatment
###################################

if (combination_trt) {
  adae <- adae |>
    mutate(
      across(
        all_of(comb_relvars),
        ~ factor(
          case_when(
            . == "NOT RELATED" ~ "Not Related",
            . == "RELATED" ~ "Related"
          ),
          levels = c(
            "Not Related",
            "Related"
          )
        )
      )
    )
}

###################################

adsl <- adsl_jnj |>
  filter(.data[[popfl]] == "Y") |>
  select(STUDYID, USUBJID, DCTADY)

adae_adsl <- adae |>
  inner_join(adsl, by = c("STUDYID" = "STUDYID", "USUBJID" = "USUBJID"))

lsting <- adae_adsl |>
  mutate(
    AGE = explicit_na(as.character(AGE), ""),
    SEX = explicit_na(SEX, ""),
    RACE = explicit_na(RACE, ""),
    DOSEON = explicit_na(as.character(DOSEON), ""),
    DOSEU = explicit_na(DOSEU, ""),
    DOSEDY = explicit_na(as.character(DOSEDY), ""),
    AEDECOD = explicit_na(AEDECOD, ""),
    AETERM = ifelse(
      is.na(AETERM),
      "",
      stringi::stri_trans_totitle(gsub("\\$", "", AETERM))
    ),
    ASTDT = ifelse(
      nchar(as.character(ASTDT)) == 10,
      toupper(format(ASTDT, "%d%b%Y")),
      ""
    ),
    ASTTM = ifelse(
      include_time & !is.na(ASTDTM),
      substr(as.character(ASTDTM), 12, 16),
      ""
    ),
    ASTDYN = ifelse(!is.na(ASTDY), ASTDY, NA),
    ASTDY = ifelse(!is.na(ASTDY), ASTDY, ""),
    ASTDTFS = ifelse(!is.na(ASTDTF), "*", ""),
    ASTDTFSC = ifelse(!is.na(ASTDTF), paste0(ASTDTF, "*"), ""),
    AENDT = explicit_na(as.character(AENDT), ""),
    AENDT = ifelse(
      nchar(AENDT) == 10,
      toupper(format(as.Date(AENDT), "%d%b%Y")),
      ""
    ),
    AENTM = ifelse(
      include_time & !is.na(AENDTM),
      substr(as.character(AENDTM), 12, 16),
      ""
    ),
    AENDY = ifelse(!is.na(AENDY), AENDY, ""),
    AENDTFS = ifelse(!is.na(AENDTF), "*", ""),
    AENDTFSC = ifelse(!is.na(AENDTF), paste0(AENDTF, "*"), ""),
    AEOUTC = explicit_na(AEOUT, ""),
    # Optional Variable: COL9/AESEV/AETOXGR
    # AESEV = explicit_na(AESEV,""),
    AETOXGR = explicit_na(AETOXGR, ""),
    AESER = explicit_na(AESER, "")
  )

if (combination_trt) {
  lsting <- lsting |>
    mutate(
      across(
        all_of(comb_dosvars),
        ~ explicit_na(as.character(.), "")
      ),
      across(
        all_of(comb_dosuvars),
        ~ explicit_na(., "")
      ),
      across(
        all_of(comb_dosdyvars),
        ~ explicit_na(as.character(.), "")
      )
    )
}

lsting <- lsting |>
  (\(x) {
    if (!combination_trt) {
      mutate(
        x,
        COL3 = paste(paste0(DOSEON, " ", DOSEU), DOSEDY, sep = concat_sep),
        COL8 = explicit_na(AEREL, "")
      )
    } else if (combination_trt) {
      rowwise(x) |>
        mutate(
          COL3 = paste(
            paste0(
              c_across(all_of(comb_trtvars)),
              ": ",
              c_across(all_of(comb_dosvars)),
              " ",
              c_across(all_of(comb_dosuvars)),
              concat_sep,
              c_across(all_of(comb_dosdyvars))
            ),
            collapse = "; "
          ),
          COL8 = paste(
            paste0(c_across(all_of(comb_trtvars)), ": ", c_across(all_of(comb_relvars))),
            collapse = "; "
          )
        )
    }
  })() |>
  mutate(
    COL0 = explicit_na(.data[[trtvar]], ""),
    COL1 = explicit_na(USUBJID, ""),
    COL2 = paste(AGE, SEX, RACE, sep = concat_sep),
    COL4 = paste(AEDECOD, AETERM, sep = concat_sep),
    COL5 = case_when(
      ASTDT == "" ~ "",
      include_time & include_imputed_dates & ASTDT != "" & ASTTM != "" & ASTDY != "" & ASTDTFSC != "" ~
        paste0(ASTDT, concat_sep, ASTTM, " (", ASTDY, ")", concat_sep, ASTDTFSC),
      include_time & include_imputed_dates & ASTDT != "" & ASTTM != "" & ASTDY != "" & ASTDTFSC == "" ~
        paste0(ASTDT, concat_sep, ASTTM, " (", ASTDY, ")"),
      include_time & include_imputed_dates & ASTDT != "" & ASTTM == "" & ASTDY != "" & ASTDTFSC != "" ~
        paste0(ASTDT, concat_sep, "--:--", " (", ASTDY, ")", concat_sep, ASTDTFSC),
      include_time & include_imputed_dates & ASTDT != "" & ASTTM == "" & ASTDY != "" & ASTDTFSC == "" ~
        paste0(ASTDT, concat_sep, "--:--", " (", ASTDY, ")"),
      include_time & ASTDT != "" & ASTTM != "" & ASTDY != "" ~ paste0(ASTDT, concat_sep, ASTTM, " (", ASTDY, ")"),
      include_time & ASTDT != "" & ASTTM == "" & ASTDY != "" ~ paste0(ASTDT, concat_sep, "--:--", " (", ASTDY, ")"),
      include_imputed_dates & ASTDT != "" & ASTDY != "" & ASTDTFSC != "" ~
        paste0(ASTDT, " (", ASTDY, ")", concat_sep, ASTDTFSC),
      include_imputed_dates & ASTDT != "" & ASTDY != "" & ASTDTFSC == "" ~ paste0(ASTDT, " (", ASTDY, ")"),
      ASTDT != "" & ASTDY != "" ~ paste0(ASTDT, " (", ASTDY, ")"),
    ),
    COL6 = case_when(
      AENDT == "" ~ "",
      include_time & include_imputed_dates & AENDT != "" & AENTM != "" & AENDY != "" & AENDTFSC != "" ~
        paste0(AENDT, concat_sep, AENTM, " (", AENDY, ")", concat_sep, AENDTFSC),
      include_time & include_imputed_dates & AENDT != "" & AENTM != "" & AENDY != "" & AENDTFSC == "" ~
        paste0(AENDT, concat_sep, AENTM, " (", AENDY, ")"),
      include_time & include_imputed_dates & AENDT != "" & AENTM == "" & AENDY != "" & AENDTFSC != "" ~
        paste0(AENDT, concat_sep, "--:--", " (", AENDY, ")", concat_sep, AENDTFSC),
      include_time & include_imputed_dates & AENDT != "" & AENTM == "" & AENDY != "" & AENDTFSC == "" ~
        paste0(AENDT, concat_sep, "--:--", " (", AENDY, ")"),
      include_time & AENDT != "" & AENTM != "" & AENDY != "" ~ paste0(AENDT, concat_sep, AENTM, " (", AENDY, ")"),
      include_time & AENDT != "" & AENTM == "" & AENDY != "" ~ paste0(AENDT, concat_sep, "--:--", " (", AENDY, ")"),
      include_imputed_dates & AENDT != "" & AENDY != "" & AENDTFSC != "" ~
        paste0(AENDT, " (", AENDY, ")", concat_sep, AENDTFSC),
      include_imputed_dates & AENDT != "" & AENDY != "" & AENDTFSC == "" ~ paste0(AENDT, " (", AENDY, ")"),
      AENDT != "" & AENDY != "" ~ paste0(AENDT, " (", AENDY, ")"),
    ),
    COL7 = case_when(
      !is.na(ADURN) & !is.na(DCTADY) ~ paste(ADURN, DCTADY, sep = concat_sep),
      !is.na(ADURN) & is.na(DCTADY) ~ paste0(ADURN, concat_sep),
      is.na(ADURN) & !is.na(DCTADY) ~ paste0(concat_sep, DCTADY),
      is.na(ADURN) & is.na(DCTADY) ~ ""
    ),
    # Optional Variable: COL9/AESEV/AETOXGR
    # COL9 = paste(AEOUTC, AESEV, AESER, sep = concat_sep)
    COL9 = paste(AEOUTC, AETOXGR, AESER, sep = concat_sep),
    # Optional Column: COL10/AESCAT
    COL10 = explicit_na(AESCAT, "")
  ) |>
  arrange(
    COL0,
    COL1,
    COL2,
    !is.na(ASTDYN),
    ASTDYN,
    if (include_time) ASTDTM else ASTDT,
    AEDECOD,
    AETERM
  )

lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  COL2 = paste("Age (years)", "Sex", "Race", sep = concat_sep),
  COL3 = paste(
    "Onset Dose (unit)",
    "Day of Last Dose~[super a,b]",
    sep = concat_sep
  ),
  COL4 = paste("Preferred Term", "Reported Term", sep = concat_sep),
  COL5 = if (include_time) {
    paste("Start Date", "Time (Study Day~[super b])", sep = concat_sep)
  } else {
    "Start Date (Study Day~[super b])"
  },
  COL6 = if (include_time) {
    paste("End Date", "Time (Study Day~[super b])", sep = concat_sep)
  } else {
    "End Date (Study Day~[super b])"
  },
  COL7 = paste(
    "AE Duration (Days)",
    "Day of Discontinuation of Study Treatment~[super b]",
    sep = concat_sep
  ),
  COL8 = "Relationship to Study Treatment~[super c]",
  # Optional Variable: COL9/AESEV/AETOXGR
  # COL9 = paste("Outcome", "Severity", "Serious", sep = concat_sep)
  COL9 = paste("Outcome", "Toxicity Grade", "Serious", sep = concat_sep),
  # Optional Column: COL10/AESCAT
  COL10 = "Event Type"
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


colwidth <- c(21, 13, 35, 23, 30, 25, 25, 38, 22, 37, 16)

tt_to_tlgrtf(head(result, 100), file = fileid, orientation = "landscape")
