library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(rlistings)
library(junco)

###############################################################################
# Define script level parameters
###############################################################################

tblid <- "LIRADA03"
fileid <- write_path(opath, tblid)
popfl <- "IMFL" # Immunogenicity Analysis Set flag
trtvar <- "TRT01A"
trtvarnum <- "TRT01AN"
adpc_paramcd_criteria <- "XAN" # based on display ATPT criteria select values accordingly (if applicable)

key_cols <- c("COL0", "COL1", "COL2")
sort_cols <- c(trtvarnum, "COL1", "AVISITN", "ATPTN")
disp_cols <- c("COL3", "COL4")

study_agent <- "active study agent"
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

###############################################################################
# Process data
###############################################################################
adishum <- adishum_jnj

# top level filter: subjects in the population
main_subject_list <- adishum |>
  filter(!!rlang::sym(popfl) == "Y") |>
  pull(USUBJID) |>
  unique()

# dataset adpc
adpc <- adpc_jnj |>
  filter(
    toupper(PARAMCD) == toupper(adpc_paramcd_criteria),
    USUBJID %in% main_subject_list
  ) |>
  select(
    USUBJID,
    AVISIT,
    AVISITN,
    ATPTN,
    ATPT,
    AVAL
  )

adishum_filtered <- adishum |>
  filter(USUBJID %in% main_subject_list) |>
  select(
    !!trtvar,
    !!trtvarnum,
    USUBJID,
    DTL
  ) |>
  unique()

# Section --- Build Dataset Base From Adishum ------------
# collect data after all joins, then apply cross-dataset filter

lsting <- adpc |>
  left_join(
    adishum_filtered,
    by = c(
      USUBJID = "USUBJID"
    )
  ) |>
  filter(AVAL > DTL)

# create necessary columns for Listing
lsting <- lsting |>
  mutate(
    COL0 = explicit_na(.data[[trtvar]]),
    COL1 = explicit_na(USUBJID, ""),
    COL2 = explicit_na(AVISIT, ""),
    # Optional Column: COL3/ATPT
    COL3 = explicit_na(ATPT, ""),
    COL4 = if_else(
      is.na(AVAL),
      "",
      formatC(AVAL, format = "fg")
    )
  )

# define column names for listing
lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  COL2 = "Visit",
  # Optional Column: COL3/ATPT
  COL3 = "Time Point",
  COL4 = sprintf(
    "Matrix %s \nConc. (mcg/mL)",
    stringi::stri_trans_totitle(study_agent)
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
  round_type = "sas",
  col_formatting = list(
    COL4 = fmt_config(
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


colwidth <- c(71, 63, 59, 60, 26)

tt_to_tlgrtf(head(result, 100), file = fileid, orientation = "landscape")
