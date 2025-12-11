###############################################################################
## Original Reporting Effort: Standards
## Program Name:              tsfae23c.R
## R Version:                 4.2.1
## Short Description:         Create: TSFAE23C: Subjects With Related
##                            Treatment-emergent Adverse Events Leading to
##                            Discontinuation of Study Treatment by Preferred
##                            Term
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      23Feb2024
## Input:                     ADAE, ADSL
## Output:                    tsfae23c.rtf
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

tblid <- "TSFAE23c"
fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01A"
aerelvar <- "AEREL"
combined_colspan_trt <- TRUE
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


if (combined_colspan_trt == TRUE) {
  # If no combined treatment column(s) needed for your study then this next
  # section of code can be removed
  # Set up levels and label for the required combined columns
  add_combo <- add_combo_facet(
    "Combined",
    label = "Combined",
    levels = c(
      "Xanomeline High Dose",
      "Xanomeline Low Dose"
    )
  )

  # choose if any facets need to be removed - e.g remove the combined column
  # for placebo
  rm_combo_from_placebo <- cond_rm_facets(
    facets = "Combined",
    ancestor_pos = NA,
    value = " ",
    split = "colspan_trt"
  )

  mysplit <- make_split_fun(post = list(add_combo, rm_combo_from_placebo))
}

###############################################################################
# Process data
###############################################################################

adsl <- adsl_jnj %>%
  filter(!!rlang::sym(popfl) == "Y") %>%
  create_colspan_var(
    non_active_grp = c("Placebo"),
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = trtvar
  ) %>%
  select(
    STUDYID,
    USUBJID,
    !!rlang::sym(popfl),
    !!rlang::sym(trtvar),
    colspan_trt
  )

trt_map <- create_colspan_map(
  df = adsl,
  non_active_grp = c("Placebo"),
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)
ref_path <- c("colspan_trt", " ", trtvar, "Placebo")

adae0 <- adae_jnj %>%
  filter(
    !!rlang::sym(popfl) == "Y" &
      !!rlang::sym(aerelvar) == "RELATED" &
      TRTEMFL == "Y" &
      AEACN == "DRUG WITHDRAWN"
  ) %>%
  left_join(
    subset(adsl, select = c("STUDYID", "USUBJID", "colspan_trt")),
    by = c("STUDYID", "USUBJID")
  ) %>%
  select(
    STUDYID,
    USUBJID,
    !!rlang::sym(trtvar),
    !!rlang::sym(popfl),
    !!rlang::sym(aerelvar),
    TRTEMFL,
    AEACN,
    AEDECOD,
    colspan_trt
  )

if (nrow(adae0) == 0) {
  adae <- adae0 %>%
    select(STUDYID, USUBJID, TRTEMFL, AEDECOD) %>%
    right_join(adsl, by = c("STUDYID", "USUBJID"))
} else {
  adae <- adae0
}

###############################################################################
# Define layout and build table
###############################################################################

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx",
  top_level_section_div = " "
) %>%
  split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = trt_map))

if (combined_colspan_trt == TRUE) {
  lyt <- lyt %>%
    split_cols_by(trtvar, split_fun = mysplit)
} else {
  lyt <- lyt %>%
    split_cols_by(trtvar)
}

lyt <- lyt %>%
  analyze(
    vars = "TRTEMFL",
    show_labels = "hidden",
    afun = a_freq_j,
    extra_args = list(
      label = "Subjects with >=1 related AE leading to discontinuation",
      .stats = c("count_unique_fraction"),
      val = "Y"
    ),
    section_div = c(" ")
  )

if (nrow(adae0) > 0) {
  lyt <- lyt %>%
    count_occurrences(
      vars = "AEDECOD",
      .stats = c("count_fraction_fixed_dp"),
      .indent_mods = c(count_fraction = -1L),
      .formats = c("count_fraction_fixed_dp" = jjcsformat_count_fraction),
      nested = FALSE
    ) %>%
    append_topleft("Preferred Term, n (%)")
} else {
  lyt <- lyt %>%
    append_topleft("Preferred Term, n (%)")
}

result <- build_table(lyt, df = adae, alt_counts_df = adsl)

###############################################################################
# Post-processing
###############################################################################

if (nrow(adae0) > 0) {
  # Post-Processing step to sort by descending count in the Combined
  # Xanomeline High Dose column
  result <- sort_at_path(
    tt = result,
    path = c("root", "AEDECOD"),
    scorefun = jj_complex_scorefun(
      spanningheadercolvar = "colspan_trt",
      colpath = NULL
    )
  )
}

if (nrow(adae0) == 0) {
  # Post-Processing step to remove 'Subjects with' row
  result <- safe_prune_table(
    result,
    prune_func = remove_rows(
      removerowtext = "Subjects with >=1 related AE leading to discontinuation"
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
