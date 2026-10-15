library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(rlistings)
library(junco)

###############################################################################
# Define script level parameters
###############################################################################

tblid <- "LSIDS02"
fileid <- write_path(opath, tblid)
popfl <- "FASFL"
trtvar <- "TRT01P"
key_cols <- c("COL0", "COL1")
sort_cols <- c("COL0", "COL1")
disp_cols <- paste0("COL", 0:9)
concat_sep <- " / "
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

# If your data has CUMDOSE and CUMDOSSx then set cumdose_components <- TRUE
cumdose_components <- FALSE

###############################################################################
# Process data
###############################################################################

adsl <- adsl_jnj |>
  filter(!!rlang::sym(popfl) == "Y" & EOSSTT %in% c("DISCONTINUED")) |>
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
  filter(PARAMCD == "CUMDOSE" | (cumdose_components & grepl("^CUMDOSS", PARAMCD))) |>
  select(STUDYID, USUBJID, PARAMCD, AVAL, PARAM) |>
  tidyr::pivot_wider(
    names_from = PARAMCD,
    values_from = c(AVAL, PARAM),
    names_glue = "{.value}_{PARAMCD}"
  )
# If your data only has CUMDOSE then this step rename AVAL to AVAL_CUMDOSE and PARAM to PARAM_CUMDOSE
# this step automatically skip if data has CUMDOSE along with CUMDOSSx
if ("AVAL" %in% names(adexsum)) {
  adexsum <- rename(adexsum, AVAL_CUMDOSE = AVAL, PARAM_CUMDOSE = PARAM)
}

adexsum <- adexsum |>
  #This step convert NA to empty string ("") to align with listing
  mutate(
    across(
      c(
        starts_with("AVAL_CUMDOSE"),
        starts_with("AVAL_CUMDOSS"),
        starts_with("PARAM_CUMDOSE"),
        starts_with("PARAM_CUMDOSS")
      ),
      ~ ifelse(is.na(.x), "", as.character(.x))
    )
  ) |>
  #Extract the unit from PARAM_CUMDOSE
  mutate(
    PARAM_CUMDOSE = case_when(
      !is.na(PARAM_CUMDOSE) | PARAM_CUMDOSE != "" ~
        stringr::str_extract(PARAM_CUMDOSE, "(?<=\\()([^()]*?)(?=\\)[^()]*$)"),
      is.na(PARAM_CUMDOSE) | PARAM_CUMDOSE == "" ~ ""
    ),
    #This step extract the study agent name and unit from PARAM_CUMDOSSx and combine with AVAL_CUMDOSSx
    across(
      starts_with("PARAM_CUMDOSS"),
      ~ {
        suffix <- sub("PARAM_", "", cur_column())
        aval_col <- paste0("AVAL_", suffix)
        aval <- pick(everything())[[aval_col]]
        #user may need to change the pattern if they have any other value in their PARAM_CUMDOSS
        agent <- trimws(ifelse(
          grepl("<[^>]*>", .x),
          sub(".*<([^>]*)>.*", "\\1", .x),
          sub("Cumulative dose\\s+(.+?)\\s+\\(.*", "\\1", .x)
        ))
        unit <- trimws(ifelse(
          grepl("\\[([^]]+)\\]", .x),
          sub(".*\\[([^]]+)\\].*", "\\1", .x),
          sub(".*\\(([^)]+)\\).*", "\\1", .x)
        ))
        ifelse(is.na(aval) | aval == "", "", paste0(agent, ": ", aval, " ", unit))
      },
      .names = "study_agent_{col}"
    ),
    #This step combine all the cumulative dose information into one column
    Cum_dose = apply(
      pick(everything()),
      1,
      function(r) {
        cumdose_part <- if (
          !is.na(r[["AVAL_CUMDOSE"]]) &&
            r[["AVAL_CUMDOSE"]] != "" &&
            !is.na(r[["PARAM_CUMDOSE"]]) &&
            r[["PARAM_CUMDOSE"]] != ""
        ) {
          paste(r[["AVAL_CUMDOSE"]], r[["PARAM_CUMDOSE"]])
        } else {
          ""
        }
        study_parts <- unname(r[grepl("^study_agent_", names(r))])
        study_parts <- study_parts[!is.na(study_parts) & study_parts != ""]
        all_parts <- c(cumdose_part, study_parts)
        all_parts <- all_parts[all_parts != ""]
        paste(all_parts, collapse = "; ")
      }
    )
  )


adsl_adexsum <- left_join(
  adsl,
  adexsum,
  by = c("STUDYID", "USUBJID")
)

lsting <- adsl_adexsum |>
  mutate(
    AGE = explicit_na(as.character(AGE), ""),
    SEX = explicit_na(SEX, ""),
    RACE = explicit_na(RACE, ""),
    DCSREAS = explicit_na(DCSREAS, ""),
    DCSREASP = explicit_na(DCSREASP, ""),
    COL0 = explicit_na(.data[[trtvar]], ""),
    COL1 = explicit_na(USUBJID, ""),
    COL2 = paste(AGE, SEX, RACE, sep = concat_sep),
    # Optional Column: COL3/LSVISIT
    COL3 = explicit_na(LSVISIT, ""),
    # Optional Column: COL4/LTVISIT
    COL4 = explicit_na(LTVISIT, ""),
    # Optional Column: COL5/TRTEDT, TRTEDY
    COL5 = ifelse(
      is.na(TRTEDT),
      "",
      toupper(format(as.Date(TRTEDT), format = "%d%b%Y"))
    ),
    # Optional Column: COL6/CUMDOSE/CUMDOSU
    COL6 = paste0(Cum_dose),
    COL7 = ifelse(
      is.na(EOSDT),
      "",
      toupper(format(as.Date(EOSDT), format = "%d%b%Y"))
    ),
    COL8 = case_when(
      DCSREAS == "OTHER" ~ paste0(DCSREAS, " (", stringr::str_to_sentence(DCSREASP), ")"),
      DCSREAS != "OTHER" ~ DCSREAS
    ),
    # Optional Column: COL9/UNBLNDFL
    COL9 = ifelse(is.na(UNBLNDFL), "No", "Yes")
  ) |>
  arrange(COL0, COL1)

lsting <- lsting |>
  mutate(
    COL5 = ifelse(is.na(TRTEDY), COL5, sprintf("%s (%s)", COL5, TRTEDY)),
    COL7 = ifelse(is.na(EOSDY), COL7, sprintf("%s (%s)", COL7, EOSDY))
  )

lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  COL2 = paste("Age (years)", "Sex", "Race", sep = concat_sep),
  # Optional Column: COL3/LSVISIT
  COL3 = "Last Study Visit~[super a]",
  #COL4 = "Study Day~[super b] of Discontinuation",
  # Optional Column: COL5/LTVISIT
  COL4 = "Last Treatment Visit~[super b]",
  # Optional Column: COL6/TRTEDY
  COL5 = "Date of Last Study Agent Administered (Study Day~[super c])",
  # Optional Column: COL7/CUMDOSE/CUMDOSU
  COL6 = "Cumulative Dose (unit)",
  COL7 = "Date of Discontinuation (Study Day~[super c])",
  COL8 = "Primary Reason for Discontinuation",
  # Optional Column: COL10/UNBLNDFL
  COL9 = "Was Blind Broken?"
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


colwidth <- c(21, 13, 67, 36, 25, 33, 20, 27, 27, 15)

tt_to_tlgrtf(head(result, 100), file = fileid, orientation = "landscape")
