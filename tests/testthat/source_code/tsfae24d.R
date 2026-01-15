################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsfae24d.R
## R version:                 4.2.1
## Short Description:         Program to create tsfae24d: Subjects with Treatment-emergent
##                            Adverse Events Toxicity Grade 3 or Greater by SOC/PT
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      27DEC2023
## Input:                     adsl.RDS, adae.RDS
## Output:                    tsfae24d.rtf
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

tblid <- "TSFAE24d"
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
# - COLSPAN_TOX: Set to "AEs" for all subjects.  Used along with the add_combo_levels()
#   split function to generate separate facets for AEs & AEs with TOXGR > 3.
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
  mutate(COLSPAN_TOX = "AEs") %>%
  select(
    USUBJID,
    !!rlang::sym(popfl),
    !!rlang::sym(trtvar),
    colspan_trt,
    COLSPAN_TOX
  )

adae <- adae_jnj %>%
  filter(TRTEMFL == "Y") %>%
  mutate(TOXGE3 = ifelse(AETOXGRN == 3 | AETOXGRN > 3, ">= 3", "< 3")) %>%
  select(USUBJID, TRTEMFL, AEBODSYS, AEDECOD, AETOXGR, TOXGE3)

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

add_active_combo <- make_split_fun(
  post = list(
    add_combo_facet(
      name = "Combined",
      label = "Combined",
      levels = c("Xanomeline High Dose", "Xanomeline Low Dose")
    ),
    cond_rm_facets(
      facets = "Combined",
      ancestor_pos = NA,
      value = " ",
      split = "colspan_trt"
    )
  )
)

# Create "TOX > 3" facet.
add_tox_levels <- make_split_fun(
  post = list(add_combo_facet(
    name = "TOXGR",
    label = ">= Grade 3 AEs",
    levels = c("AEs")
  ))
)

extra_args1 <- list(
  .stats = c("count_unique_fraction"),
  subcol_split = "TOXGR",
  subcol_var = "TOXGE3",
  subcol_val = ">= 3"
)

lyt <- basic_table(top_level_section_div = " ") %>%
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) %>%
  split_cols_by(
    trtvar,
    split_fun = add_active_combo,
    show_colcounts = TRUE,
    colcount_format = "N=xx"
  ) %>%
  split_cols_by("COLSPAN_TOX", split_fun = add_tox_levels) %>%
  analyze(
    "TRTEMFL",
    nested = FALSE,
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
# Sort by descending AEBODSYS/AEDECOD in combined TOXGR AE column
# Prune table to only keep those that meet x% criteria
################################################################################

result <- result %>%
  sort_at_path(
    path = c("AEBODSYS"),
    scorefun = cont_n_onecol("Active Study Agent.Combined.TOXGR")
  ) %>%
  sort_at_path(
    path = c("AEBODSYS", "*", "AEDECOD"),
    scorefun = score_occurrences_cols("Active Study Agent.Combined.TOXGR")
  )

row_condition <- has_fraction_in_any_col(
  atleast = .02,
  col_names = c(
    "Active Study Agent.Xanomeline High Dose.TOXGR",
    "Active Study Agent.Xanomeline Low Dose.TOXGR",
    "Active Study Agent.Combined.TOXGR",
    " .Placebo.TOXGR"
  )
)

result <- safe_prune_table(result, keep_rows(row_condition))

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table:
################################################################################

tt_to_tlgrtf(
  result,
  file = fileid,
  orientation = "landscape",
  nosplitin = list(cols = c(trtvar))
)
