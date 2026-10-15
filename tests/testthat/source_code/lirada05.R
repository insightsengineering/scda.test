library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(rlistings)
library(junco)

###############################################################################
# Define script level parameters
###############################################################################

tblid <- "LIRADA05"
fileid <- write_path(opath, tblid)
popfl <- "IMFL" # Immunogenicity Analysis Set flag
trtvar <- "TRT01A"
trtvarnum <- "TRT01AN"

key_cols <- c("COL0", "COL1", "COL2", "COL3")
sort_cols <- c(trtvarnum, "COL1", "AVISITN", "ATPTN")
disp_cols <- paste0("COL", 4:7)

study_agent <- "active study agent"
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

###############################################################################
# Process data
###############################################################################

adishum <- adishum_jnj

# top level filter
main_subject_list <- adishum |>
  filter(
    !!rlang::sym(popfl) == "Y"
  ) |>
  pull(
    USUBJID
  ) |>
  unique()

# base: visit-level, one row per subject-visit-timepoint
adishum_filtered_main <- adishum |>
  filter(
    USUBJID %in% main_subject_list,
    !is.na(AVISIT)
  ) |>
  select(
    !!trtvar,
    !!trtvarnum,
    AVISIT,
    AVISITN,
    USUBJID,
    ATPT,
    ATPTN,
    ADA_STATUS = ADATRES,
    NAB_STATUS = NABSTAT
  ) |>
  distinct()

# adishum_filtered on titer
adishum_filtered_titer <- adishum |>
  filter(
    stringi::stri_trans_toupper(PARAMCD) == "TITER",
    USUBJID %in% main_subject_list
  ) |>
  select(
    USUBJID,
    AVISIT,
    TITER_AVAL = AVAL
  )

# adishum_filtered on NABCNR
adishum_filtered_nabcnr <- adishum |>
  filter(
    stringi::stri_trans_toupper(PARAMCD) == "NABCNR",
    USUBJID %in% main_subject_list
  ) |>
  select(
    USUBJID,
    AVISIT,
    NABCNR_AVALC = AVALC
  )

# Section --- Build Dataset Base From Adishum ------------
# collect data after all joins
lsting <- adishum_filtered_main |>
  left_join(adishum_filtered_titer, by = c("USUBJID", "AVISIT")) |>
  left_join(adishum_filtered_nabcnr, by = c("USUBJID", "AVISIT"))

# create necessary columns for Listing
lsting <- lsting |>
  mutate(
    COL0 = explicit_na(.data[[trtvar]]),
    COL1 = explicit_na(USUBJID, ""),
    COL2 = explicit_na(ADA_STATUS, ""),
    # Optional Column: COL3/NAB_STATUS
    COL3 = explicit_na(NAB_STATUS, ""),
    COL4 = explicit_na(AVISIT, ""),
    # Optional Column: COL5/ATPT
    COL5 = explicit_na(ATPT, ""),
    COL6 = if_else(
      is.na(TITER_AVAL),
      "",
      formatC(TITER_AVAL, format = "fg")
    ),
    # Optional Column: COL7/NABCNR_AVALC
    COL7 = explicit_na(NABCNR_AVALC, "")
  )

# define column names for listing
lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  COL2 = "Subject Status~[super a] for Treatment-Emergent ADA",
  # Optional Column: COL3/NAB_STATUS
  COL3 = "Subject Status for NAb",
  COL4 = "Visit",
  # Optional Column: COL5/ATPT
  COL5 = "Time Point",
  COL6 = sprintf(
    "Antibody to %s Status/Titer",
    study_agent
  ),
  # Optional Column: COL7/NABCNR_AVALC
  COL7 = "Sample Status for NAb"
)

###############################################################################
# Build listing
###############################################################################

result <- rlistings::as_listing(
  df = lsting,
  key_cols = key_cols,
  disp_cols = disp_cols,
  sort_cols = sort_cols,
  round_type = "sas",
  col_formatting = list(
    COL6 = fmt_config(
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


colwidth <- c(39, 38, 25, 21, 59, 60, 29, 14)

tt_to_tlgrtf(head(result, 100), file = fileid, orientation = "landscape")
