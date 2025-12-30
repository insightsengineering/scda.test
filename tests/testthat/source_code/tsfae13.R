################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsfae13.R
## R version:                 4.2.1
## Short Description:         Program to create tsfae13: Exposure-adjusted Incidence
##                            Rate Analysis by Preferred Term
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      15FEB2024
## Input:                     adsl.RDS, adae.RDS
## Output:                    tsfae13.rtf
## Remarks:
##
## Modification History:
##  Rev #:
##  Modified By:
##  Reporting Effort:
##  Date:
##  Description:
################################################################################

################################################################################
# Prep environment:
################################################################################

library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSFAE13"
fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)


################################################################################
# Process data:
################################################################################

adexsum <- adexsum_jnj %>%
  filter(!!rlang::sym(popfl) == "Y" & PARAMCD == "TRTDURY") %>%
  create_colspan_var(
    non_active_grp = ctrl_grp,
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = trtvar
  ) %>%
  mutate(
    rrisk_header = "Risk Difference (95% CI)",
    rrisk_label = paste(!!rlang::sym(trtvar), "vs", ctrl_grp),
    TRTDURY = AVAL
  ) %>%
  select(
    USUBJID,
    !!rlang::sym(trtvar),
    colspan_trt,
    rrisk_header,
    rrisk_label,
    TRTDURY
  )

adae <- adae_jnj %>%
  filter(TRTEMFL == "Y" & AOCCPFL == "Y") %>%
  select(USUBJID, AEDECOD, ASTDY, AOCCPFL)

#  join -- -- subjects without ae will be handled via alt_counts_df dataframe
aefup <- right_join(adae, adexsum, by = "USUBJID")

colspan_trt_map <- create_colspan_map(
  adexsum,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)
ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)

################################################################################
# Define layout and build table:
################################################################################

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx",
  top_level_section_div = " "
) %>%
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) %>%
  split_cols_by(trtvar) %>%
  split_cols_by("rrisk_header", nested = FALSE) %>%
  split_cols_by(
    trtvar,
    labels_var = "rrisk_label",
    split_fun = remove_split_levels(ctrl_grp)
  ) %>%
  analyze(
    "TRTDURY",
    nested = FALSE,
    show_labels = "hidden",
    afun = a_patyrs_j,
    extra_args = list(.labels = c(patyrs = "Subject years\u1D43"))
  ) %>%
  analyze(
    vars = "AEDECOD",
    nested = FALSE,
    afun = a_eair100_j,
    extra_args = list(
      fup_var = "TRTDURY",
      occ_var = "AOCCPFL",
      occ_dy = "ASTDY",
      ref_path = ref_path,
      drop_levels = TRUE
    )
  ) %>%
  append_topleft("Preferred Term, EAIR Per 100 SY")


result <- build_table(lyt, aefup, alt_counts_df = adexsum)


################################################################################
# Post-Processing:
# - Remove Ns from Risk cols
# - Sort by descending AEDECOD in the combined Xanomeline High Dose column
################################################################################
result <- result %>%
  sort_at_path(
    path = c("AEDECOD"),
    scorefun = jj_complex_scorefun(colpath = "Xanomeline High Dose")
  )

result <- remove_col_count(result)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table:
################################################################################

tt_to_tlgrtf(result, file = fileid, orientation = "landscape")
