################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsfae24a.R
## R version:                 4.2.1
## Short Description:         Program to create tsfae24a: Overall Summary of
##                            Treatment-emergent Adverse Events by Severity
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      13DEC2023
## Input:                     adsl.RDS, adae.RDS
## Output:                    tsfae24a.rtf
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

tblid <- "TSFAE24a"
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
  select(USUBJID, !!rlang::sym(trtvar), colspan_trt)

adae <- adae_jnj %>%
  filter(TRTEMFL == "Y") %>%
  mutate(
    ASEV = factor(
      ifelse(is.na(AESEV), "Missing", as.character(AESEV)),
      levels = c("Severe", "Moderate", "Mild", "Missing")
    )
  ) %>%
  select(USUBJID, TRTEMFL, ASEV)

ae <- inner_join(adae, adsl, by = c("USUBJID"))

# Keep maximum severity per subject.
ae <- ae %>%
  arrange(USUBJID, ASEV) %>%
  group_by(USUBJID) %>%
  slice(1) %>%
  ungroup()

################################################################################
# Define layout and build table:
################################################################################
extra_args_1 <- list(.stats = c("count_unique_fraction"))

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = "Placebo",
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)


lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx",
  top_level_section_div = " "
) %>%
  append_topleft("n (%)") %>%
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) %>%
  split_cols_by(trtvar) %>%
  analyze_num_patients(
    vars = "USUBJID",
    .stats = "unique",
    .formats = c("unique" = jjcsformat_count_fraction),
    .labels = "Subjects with >= 1 AE"
  ) %>%
  analyze(
    "ASEV",
    afun = a_freq_j,
    extra_args = extra_args_1,
    var_labels = "Severity",
    nested = FALSE,
    show_labels = "visible",
  )

result <- build_table(lyt, ae, alt_counts_df = adsl)

################################################################################
# Prune missing category if all zeros:
################################################################################

result <- safe_prune_table(
  result,
  prune_func = count_pruner(cat_include = "Missing")
)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table:
################################################################################

tt_to_tlgrtf(result, file = fileid)
