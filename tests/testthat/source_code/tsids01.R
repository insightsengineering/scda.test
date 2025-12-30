###############################################################################
## Original Reporting Effort: Standards
## Program Name:              tsids01.R
## R Version:                 4.2.1
## Short Description:         Create TSIDS01:	Subject Screening and Enrollment
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      23Feb2024
## Input:                     ADSL
## Output:                    tsids01.rtf
## Remarks:
## R-functions:
## R-function Sample Call:
##
## Modification History:
##  Rev #:
##  Modified By:
##  Reporting Effort:
##  Date:
##  Description:
###############################################################################

###############################################################################
# Prep environment
###############################################################################

library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)

###############################################################################
# Define script level parameters
###############################################################################

tblid <- "TSIDS01"
fileid <- write_path(opath, tblid)
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)


###############################################################################
# Process data
###############################################################################

adsl <- adsl_jnj %>%
  filter(SCRNFL == "Y") %>%
  mutate(
    SCRNFL = with_label(.data[["SCRNFL"]] == "Y", "Patients screened"),
    SCRFFL = with_label(.data[["SCRFFL"]], "Screening failures"),
    RESCRNFL = with_label(.data[["RESCRNFL"]] == "Y", "Subjects re-screened"),
    ENRLFL = with_label(.data[["ENRLFL"]] == "Y", "Subjects enrolled"),
    RANDFL = with_label(.data[["RANDFL"]] == "Y", "Subjects randomized")
  ) %>%
  select(
    STUDYID,
    USUBJID,
    SCRNFL,
    SCRFFL,
    DCSCREEN,
    RESCRNFL,
    RANDFL,
    ENRLFL,
    TRT01P
  )

adsl_unq <- adsl %>%
  distinct(STUDYID, USUBJID, .keep_all = TRUE)

###############################################################################
# Define layout and build table
###############################################################################

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx",
  top_level_section_div = " "
) %>%
  add_overall_col(label = "Total") %>%
  split_rows_by("SCRFFL", split_fun = keep_split_levels("Y")) %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = "unique",
    .formats = c("unique" = jjcsformat_count_fraction),
    .labels = c(unique = "Screening failures")
  ) %>%
  count_occurrences(
    vars = "DCSCREEN",
    drop = FALSE,
    .stats = "count_fraction_fixed_dp",
    .formats = c("count_fraction_fixed_dp" = jjcsformat_count_fraction)
  ) %>%
  count_patients_with_flags(
    var = "USUBJID",
    flag_variables = c("RESCRNFL"),
    nested = FALSE,
    .stats = "count"
  ) %>%
  count_patients_with_flags(
    var = "USUBJID",
    flag_variables = c("RANDFL"),
    nested = FALSE,
    .stats = "count_fraction",
    .formats = c("count_fraction" = jjcsformat_count_fraction)
  )

result <- build_table(lyt, df = adsl, alt_counts_df = adsl_unq)

###############################################################################
# Post-processing
###############################################################################

# Post-processing step to sort by descending count in the Combined column
result <- sort_at_path(
  tt = result,
  path = c("root", "SCRFFL", "Y", "DCSCREEN"),
  scorefun = jj_complex_scorefun(
    spanningheadercolvar = NULL,
    colpath = c("Total", "Total"),
    firstcat = NULL,
    lastcat = "Other"
  )
)

if (nrow(adsl) == 0) {
  # Post-processing step to remove table rows with all 0 or NA values
  result <- safe_prune_table(result, prune_func = prune_empty_level)
} else {
  # Post-processing step to remove reason for screening failures table rows
  # with all 0 or NA values
  result <- prune_table(
    result,
    prune_func = count_pruner(
      cat_exclude = c(
        "Screening failures"
      ),
      cols = "Total"
    )
  )
  result <- prune_table(
    result,
    prune_func = count_pruner(
      cat_exclude = c(
        "Subjects re-screened"
      ),
      cols = "Total"
    )
  )
  result <- prune_table(
    result,
    prune_func = count_pruner(
      cat_exclude = c(
        "Subjects randomized"
      ),
      cols = "Total"
    )
  )
}

###############################################################################
# Retrieve titles and footnotes
###############################################################################

result <- set_titles(result, tab_titles)

###############################################################################
# Convert to tbl file and output table
###############################################################################

tt_to_tlgrtf(result, file = fileid)
