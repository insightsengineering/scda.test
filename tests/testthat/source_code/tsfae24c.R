################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsfae24c.R
## R version:                 4.2.1
## Short Description:         Program to create tsfae24c: Subjects with
##                            Treatment-emergent Adverse Events and Related
##                            Treatment-emergent Adverse Events by SOC/PT
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      19DEC2023
## Input:                     adsl.RDS, adae.RDS
## Output:                    tsfae24c.rtf
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

tblid <- "TSFAE24c"
fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01A"
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)


################################################################################
# Process data:
# Create variables for column spanning headers:
# - colspan_trt: Used to generate the "Active Study Agent" spanning header.
# - COLSPAN_REL: Set to "AEs" for all subjects.  Used along with the add_combo_levels()
#   split function to generate separate facets for AEs & Related AEs.
################################################################################

adsl <- adsl_jnj %>%
  filter(!!rlang::sym(popfl) == "Y") %>%
  create_colspan_var(
    non_active_grp = "Placebo",
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = trtvar
  ) %>%
  mutate(COLSPAN_REL = "AEs") %>%
  select(
    USUBJID,
    !!rlang::sym(popfl),
    !!rlang::sym(trtvar),
    colspan_trt,
    COLSPAN_REL
  )

adae <- adae_jnj %>%
  filter(TRTEMFL == "Y") %>%
  select(USUBJID, TRTEMFL, AEBODSYS, AEDECOD, AEREL)

adae <- inner_join(adae, adsl, by = c("USUBJID"))

################################################################################
# Define layout and build table:
################################################################################

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = "Placebo",
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

combodf <- tribble(
  ~valname, ~label, ~levelcombo, ~exargs,
  "RELATED", "Related AEs", c("AEs"), list()
)

extra_args1 <- list(
  .stats = c("count_unique_fraction"),
  subcol_split = "RELATED",
  subcol_var = "AEREL",
  subcol_val = "RELATED"
)

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx",
  top_level_section_div = " "
) %>%
  split_cols_by(
    "COLSPAN_REL",
    split_fun = add_combo_levels(combodf, trim = TRUE)
  ) %>%
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) %>%
  split_cols_by(trtvar) %>%
  analyze(
    "TRTEMFL",
    afun = a_freq_subcol_j,
    extra_args = append(
      extra_args1,
      list(
        label = "Subjects with >= 1 AE",
        val = "Y"
      )
    )
  ) %>%
  split_rows_by(
    "AEBODSYS",
    split_label = "System Organ Class",
    label_pos = "topleft",
    split_fun = trim_levels_in_group("AEDECOD"),
    section_div = " ",
    nested = FALSE
  ) %>%
  summarize_row_groups(
    "AEBODSYS",
    cfun = a_freq_subcol_j,
    extra_args = extra_args1
  ) %>%
  analyze("AEDECOD", afun = a_freq_subcol_j, extra_args = extra_args1) %>%
  append_topleft(" Preferred Term, n (%)")

result <- build_table(lyt, adae, alt_counts_df = adsl)

################################################################################
# Post-Processing:
# Sort by descending AEBODSYS/AEDECOD in combined AE column
################################################################################

result <- result %>%
  sort_at_path(
    path = c("AEBODSYS"),
    scorefun = cont_n_onecol("AEs.Active Study Agent.Xanomeline High Dose")
  ) %>%
  sort_at_path(
    path = c("AEBODSYS", "*", "AEDECOD"),
    scorefun = score_occurrences_cols(
      "AEs.Active Study Agent.Xanomeline High Dose"
    )
  )

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table:
################################################################################

tt_to_tlgrtf(result, file = fileid, orientation = "landscape")
