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

tblid <- "LSIEX04"
fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01A"
key_cols <- c("COL0", "COL1", "COL2")
sort_cols <- c("COL0", "COL1", "COL2")
disp_cols <- paste0("COL", 0:13)
concat_sep <- " / "
end_time <- TRUE
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


###############################################################################
# Process data
###############################################################################

adex <- adex_jnj |>
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
        SEX == "M" ~ "Male",
        SEX == "F" ~ "Female",
        TRUE ~ SEX
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
    )
  ) |>
  filter(!!rlang::sym(popfl) == "Y")

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
    ASCHDOSU = explicit_na(ASCHDOSU, ""),
    AVAMTU = explicit_na(AVAMTU, ""),
    ACDOSU = explicit_na(ACDOSU, ""),
    ADOSU = explicit_na(ADOSU, ""),
    ASTDT = ifelse(
      !is.na(ASTDT) & nchar(as.character(ASTDT)) == 10,
      toupper(format(ASTDT, "%d%b%Y")),
      ""
    ),
    ASTDY = ifelse(!is.na(ASTDY), ASTDY, ""),
    ASTDYL = ifelse(!is.na(ASTDT) & is.na(ASTDY), "-", as.character(ASTDY)),

    ASTTM = ifelse(!is.na(ASTDTM), substr(as.character(ASTDTM), 12, 16), ""),
    AENTM = if_else(end_time & !is.na(AENDTM), substr(as.character(AENDTM), 12, 16), ""),
    preas = case_when(
      toupper(AADJP) == "ADVERSE EVENT" ~ paste0(stringr::str_to_sentence(AADJP), " (AE: ", decod, ")"),
      toupper(AADJP) == "OTHER" ~
        paste0(
          stringr::str_to_sentence(AADJP),
          ": ",
          stringr::str_to_sentence(AADJPOTH)
        ),
      !is.na(AADJP) ~ stringr::str_to_sentence(AADJP),
      TRUE ~ ""
    ),
    dreas = case_when(
      toupper(AADJ) == "ADVERSE EVENT" ~ paste0(stringr::str_to_sentence(AADJ), " (AE: ", decod, ")"),
      toupper(AADJ) == "OTHER" ~
        paste0(
          stringr::str_to_sentence(AADJ),
          ": ",
          stringr::str_to_sentence(AADJOTH)
        ),
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
    COL3 = explicit_na(stringr::str_to_sentence(AVISIT), ""),
    # Prescribed: COL4/ASCHDOSE/ASCHDOSU
    COL4 = ifelse(is.na(ASCHDOSE), "", paste(ASCHDOSE, ASCHDOSU)),
    # Prescribed: COL5/ADOSE/ADOSU (Injection Volume)
    COL5 = ifelse(is.na(AVAMT), "", AVAMT),
    # Prior to Injection Start: COL6/AACTPR/AADJP/AADJPOTH (Action Taken / Reason)
    COL6 = case_when(
      !is.na(AACTPR) ~ paste(stringr::str_to_sentence(AACTPR), preas, sep = concat_sep),
      !is.na(AACTPR) & is.na(preas) ~ paste(stringr::str_to_sentence(AACTPR), "", sep = concat_sep),
      TRUE ~ ""
    ),
    # Optional column Prior to Injection Start: COL7/Schedule Change New Dosing Frequency
    COL7 = explicit_na(ADOSFRQP, ""),
    # Optional Prior to Injection Start: COL8/Dose Delayed? / Reason
    COL8 = case_when(
      !is.na(ADOSDLY) & dly_reas != "" ~ paste(stringr::str_to_sentence(ADOSDLY), dly_reas, sep = concat_sep),
      !is.na(ADOSDLY) & dly_reas == "" ~ paste(stringr::str_to_sentence(ADOSDLY)),
      TRUE ~ ""
    ),
    # During Injection: COL9/AACTDU1-AACTDU5/AADJ/AADJOTH (Action Taken / Reason)
    COL9 = case_when(
      !is.na(AACTDU) & dreas != "" ~ paste(stringr::str_to_sentence(AACTDU), dreas, sep = concat_sep),
      !is.na(AACTDU) & dreas == "" ~ paste(stringr::str_to_sentence(AACTDU), "", sep = concat_sep),
      TRUE ~ ""
    ),
    COL10 = ifelse(is.na(ACDOSE), "", ACDOSE),
    COL11 = ifelse(is.na(ADOSE), "", paste(ADOSE, ADOSU)),
    # Start Date (Study Day)
    COL12 = case_when(
      !is.na(ASTDT) & !is.na(ASTDYL) ~ paste0(toupper(ASTDT), " (", ASTDYL, ")"),
      !is.na(ASTDT) & is.na(ASTDYL) ~ toupper(ASTDT),
      TRUE ~ ""
    ),

    COL13 = case_when(
      !end_time & ASTTM != "" ~ ASTTM,
      !end_time ~ "",
      ASTTM == "" & AENTM == "" ~ "",
      ASTTM != "" & AENTM != "" ~ paste0(ASTTM, concat_sep, AENTM),
      ASTTM != "" & AENTM == "" ~ paste0(ASTTM, concat_sep, "--:--"),
      ASTTM == "" & AENTM != "" ~ paste0("--:--", concat_sep, AENTM),
      TRUE ~ ""
    )
  ) |>
  arrange(
    COL0,
    COL1,
    COL2
  )

lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  COL2 = paste("Age (years)", "Sex", "Race", sep = concat_sep),
  # Optional Column: COL3/AVISIT
  COL3 = "Visit",
  COL4 = "Dose [Level] (unit)",
  COL5 = "Injection Volume (mL)",
  COL6 = paste("Action Taken~[super a]", "Reason", sep = concat_sep),
  # Optional Prior to Injection Start: Schedule Change New Dosing Frequency
  COL7 = "Schedule Change New Dosing Frequency",
  # Optional Prior to Injection Start: Dose Delayed? / Reason
  COL8 = paste("Dose Delayed?", "Reason", sep = concat_sep),
  COL9 = paste("Action Taken", "Reason", sep = concat_sep),
  COL10 = "Volume of Investigation Product Injected (mL)",
  COL11 = "Actual Dose Administered (unit)",
  COL12 = "Start Date (Study Day~[super b])",
  COL13 = if (end_time) paste("Start Time", "End Time", sep = concat_sep) else "Start Time"
)

###############################################################################
# Build listing
###############################################################################

spanning_labels <- data.frame(
  span_level = 1L,
  label = c("Prescribed", "Prior to Injection Start", "During Injection"),
  start = c(5L, 7L, 10L),
  span = c(2L, 2L, 1L)
)

result <- rlistings::as_listing(
  df = lsting,
  key_cols = key_cols,
  disp_cols = disp_cols,
  spanning_col_labels = spanning_labels,
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


colwidth <- c(21, 13, 17, 17, 18, 18, 32, 18, 32, 33, 22, 23, 23, 13)

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
