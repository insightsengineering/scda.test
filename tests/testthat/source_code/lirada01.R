library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(rlistings)
library(junco)

###############################################################################
# Define script level parameters
###############################################################################

tblid <- "LIRADA01"
fileid <- write_path(opath, tblid)
popfl <- "IMFL" # Immunogenicity Analysis Set flag
trtvar <- "TRT01A"
trtvarnum <- "TRT01AN"
adpc_paramcd_criteria <- "DOSE"
parqual_value <- "XANOMELINE"
sev_grade_fl <- "AESEV" # set to "AETOXGR" for toxicity grade
# sev_grade_fl <- "AETOXGR"
# ADAE AESCAT filter — user updates list as needed
ae_scat_values <- c(
  "Infusion related reaction",
  "Infusion site reaction",
  "Injection site reaction"
)

key_cols <- c("COL0", "COL1", "COL2")
sort_cols <- c(trtvarnum, "COL1", "AVISITN")
disp_cols <- paste0("COL", 0:9)
study_specific_col <- c(
  "COL9",
  "COL10"
)

study_agent <- "active study agent"
agent_antibody_label <- sprintf("antibodies to %s", study_agent)
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")
###############################################################################
# Process data
###############################################################################

# Read adishum with population and PARQUAL filters
adishum <- adishum_jnj |> 
  filter(
    !!rlang::sym(popfl) == "Y",
    toupper(PARQUAL) == toupper(parqual_value)
  )

# top level filter — ADA-positive subjects
main_subject_list <- adishum |>
  filter(
    toupper(PARAMCD) == "ADATRE",
    toupper(AVALC) == "Y"
  ) |>
  pull(USUBJID) |>
  unique()

# Read all adex records for ADA-positive subjects
adex <- adex_jnj |>
  filter(USUBJID %in% main_subject_list) |>
  select(USUBJID, AVISIT, EXSTDT, AOCCUR)

# Read adae filtered by AESCAT and ADA-positive subjects
adae <- adae_jnj |>
  filter(
    USUBJID %in% main_subject_list,
    toupper(AESCAT) %in% toupper(ae_scat_values)
  ) |>
  select(
    USUBJID,
    DOSEDT,
    AESER,
    AESEV,
    AESEVN,
    AETOXGR,
    AETOXGRN
  ) |>
  mutate(react_flg = "Y")

# Derive per-subject flags from ADAE records linked to ADEX via USUBJID + date
# DOSEDT is SAS numeric date (double); SAS origin is 1960-01-01
adae_linked <- adae |>
  mutate(
    DOSEDT = as.Date(DOSEDT, origin = "1960-01-01")
  ) |>
  inner_join(
    adex |>
      select(USUBJID, EXSTDT, AVISIT),
    by = c("USUBJID", "DOSEDT" = "EXSTDT")
  )

# ADEX visit-level AOCCUR — join to lsting by USUBJID + AVISIT
adex_visit <- adex |>
  select(USUBJID, AVISIT, AOCCUR)

# One row per subject+visit — reaction flag Y if any linked AE exists
react_flg_df <- adae_linked |>
  distinct(USUBJID, AVISIT, react_flg)

# One row per subject+visit — AESER=Y if any linked AE is serious
aeser_flg_df <- adae_linked |>
  group_by(USUBJID, AVISIT) |>
  summarise(
    aeser_flg = if_else(any(toupper(AESER) == "Y"), "Y", "N"),
    .groups = "drop"
  )

# One row per subject+visit — max severity label from linked AEs
# uses AESEV when sev_grade_fl = "AESEV", else AETOXGR
aesev_flg_df <- adae_linked |>
  group_by(USUBJID) |>
  summarise(
    max_sev_flg = if (sev_grade_fl == "AESEV") {
      AESEV[which.max(AESEVN)]
    } else {
      AETOXGR[which.max(AETOXGRN)]
    },
    .groups = "drop"
  )

# Read adpc PK concentration for ADA-positive subjects
adpc <- adpc_jnj |>
  filter(
    toupper(PARAMCD) == toupper(adpc_paramcd_criteria),
    USUBJID %in% main_subject_list
  ) |>
  select(
    USUBJID,
    adpc_AVISIT = AVISIT,
    adpc_AVAL = AVAL
  )

adishum_filtered_titer <- adishum |>
  filter(
    toupper(PARAMCD) == "TITER",
    USUBJID %in% main_subject_list
  ) |>
  select(
    !!trtvar,
    !!trtvarnum,
    AVISIT,
    AVISIT,
    AVISITN,
    USUBJID,
    TITER_AVAL = AVAL
  )

# Section --- Build Dataset Base From Adishum ------------
# collect data after all joins; adex/react_fl_df/aeser_df/aesev_df are
# one row per subject — no many-to-many risk
lsting <- adishum_filtered_titer |>
  left_join(
    adpc,
    by = c("USUBJID", "AVISIT" = "adpc_AVISIT")
  ) |>
  left_join(adex_visit, by = c("USUBJID", "AVISIT")) |>
  left_join(react_flg_df, by = c("USUBJID", "AVISIT")) |>
  left_join(aeser_flg_df, by = c("USUBJID", "AVISIT")) |>
  left_join(aesev_flg_df, by = "USUBJID")

# create necessary columns for Listing
lsting <- lsting |>
  mutate(
    COL0 = explicit_na(.data[[trtvar]]),
    COL1 = explicit_na(USUBJID, ""),
    COL2 = explicit_na(AVISIT, ""),
    # STUDY-SPECIFIC: update to Injection, Infusion, or Dose as appropriate
    COL3 = explicit_na(AOCCUR, ""),
    # Optional Column: COL4/react_flg
    COL4 = if_else(!is.na(react_flg), react_flg, ""),
    # Optional Column: COL5/aeser_flg
    COL5 = if_else(!is.na(aeser_flg), aeser_flg, ""),
    # Optional Column: COL6/max_sev_flg
    COL6 = if_else(!is.na(max_sev_flg), max_sev_flg, ""),
    COL7 = if_else(
      is.na(adpc_AVAL),
      "",
      formatC(adpc_AVAL, format = "fg")
    ),
    COL8 = if_else(
      is.na(TITER_AVAL),
      "",
      formatC(TITER_AVAL, format = "fg")
    ) #,
    # STUDY-SPECIFIC: replace later
    # COL9 = "STUDY-SPECIFIC",
    # COL10 = "STUDY-SPECIFIC"
  )

# define column names for listing
lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  COL2 = "Visit",
  # STUDY-SPECIFIC: update label to Injection, Infusion, or Dose as appropriate
  COL3 = "[Injection / Infusion / Dose] Given?",
  # Optional Column: COL4/react_flg
  COL4 = "[Injection Site / Infusion-related] Reaction",
  # Optional Column: COL5/aeser_flg
  COL5 = "Serious",
  # Optional Column: COL6/max_sev_flg
  COL6 = "Maximum Severity",
  COL7 = sprintf(
    "Matrix %s Conc. (mcg/mL)",
    stringi::stri_trans_totitle(study_agent)
  ),
  COL8 = sprintf(
    "%s Status/Titer",
    stringi::stri_trans_totitle(agent_antibody_label)
  ) #,
  # STUDY-SPECIFIC: update label to Success Criteria, Continuous Response, or Concomitant Medications
  # COL9 = "[Success Criteria / Continuous Response]"
  # COL10 = "[Success Criteria / Concomitant Medications]"
)

###############################################################################
# Build listing
###############################################################################

result <- rlistings::as_listing(
  df = lsting,
  key_cols = key_cols,
  disp_cols = disp_cols[!disp_cols %in% study_specific_col],
  sort_cols = sort_cols,
  round_type = "sas",
  col_formatting = list(
    COL7 = fmt_config(
      na_str = "",
      align = "right"
    ),
    COL8 = fmt_config(
      na_str = "",
      align = "right"
    )
  )
)

###############################################################################
# Add titles and footnotes
###############################################################################

result <- set_titles(result, tab_titles)

###############################################################################
# Output listing
###############################################################################


colwidth <- c(21, 63, 59, 23, 27, 13, 18, 23, 30)

tt_to_tlgrtf(head(result, 100), file = fileid, orientation = "landscape")
