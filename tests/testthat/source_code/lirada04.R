library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(rlistings)
library(junco)

###############################################################################
# Define script level parameters
###############################################################################

tblid <- "LIRADA04"
fileid <- write_path(opath, tblid)
popfl <- "IMFL" # Immunogenicity Analysis Set flag
trtvar <- "TRT01A"
trtvarnum <- "TRT01AN"

parqual_value <- "XANOMELINE"
key_cols <- c("COL0", "COL1")
sort_cols <- c(trtvarnum, "COL1", "ADT")
disp_cols <- "COL2"

tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

###############################################################################
# Process data
###############################################################################

adishum <- adishum_jnj

# top level filter
main_subject_list <- adishum |>
  filter(
    !!rlang::sym(popfl) == "Y",
    PARQUAL == parqual_value,
    PARAMCD == "ADABL",
    AVALC == "Y"
  ) |>
  pull(
    USUBJID
  ) |>
  unique()

# adishum_filtered on ADABLT
adishum_filtered_adablt <- adishum |>
  filter(
    stringi::stri_trans_toupper(PARAMCD) == "ADABLT",
    USUBJID %in% main_subject_list
  ) |>
  select(
    !!trtvar,
    !!trtvarnum,
    ADT,
    USUBJID,
    ADABLT_AVAL = AVAL
  )

# Section --- Build Dataset Base From Adishum ------------
# collect data after all joins
lsting <- adishum_filtered_adablt

# create necessary columns for Listing
lsting <- lsting |>
  mutate(
    COL0 = explicit_na(.data[[trtvar]]),
    COL1 = explicit_na(USUBJID, ""),
    COL2 = if_else(
      is.na(ADABLT_AVAL),
      "",
      formatC(ADABLT_AVAL, format = "fg")
    )
  )

# define column names for listing
lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  COL2 = "Baseline Titer"
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
    COL2 = fmt_config(
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


colwidth <- c(71, 63, 15)

tt_to_tlgrtf(head(result, 100), file = fileid, orientation = "landscape")
