################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsfae20a.R
## R version:                 4.2.1
## Short Description:         Program to create tsfae20a: Summary of Demographic
##                            Characteristics for Subjects With Treatment-emergent
##                            Adverse Events
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      07 Dec 2023
## Input:                     adsl.rds, adae.rds
## Output:                    tsfae20a.rtf
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
# Prep Environment
################################################################################

library(envsetup)
library(tern)
library(forcats)
library(dplyr)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSFAE20a"
titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01A"
subjFilterText <- "AE"
ctrl_grp <- "Placebo"

################################################################################
# Process Data
# - Sub-setting performed on a subject-level; certain tables may require a more
#   granular, column-based sub-setting.
# - Factor for Active Treatment spanning header added below.
# - Additional factor reformatting added below.
################################################################################

adsl <- adsl_jnj %>%
  filter(!!rlang::sym(popfl) == "Y") %>%
  create_colspan_var(
    non_active_grp = ctrl_grp,
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = trtvar
  ) %>%
  select(
    USUBJID,
    !!rlang::sym(popfl),
    !!rlang::sym(trtvar),
    SEX_DECODE,
    AGEGR1,
    RACE_DECODE,
    ETHNIC_DECODE,
    colspan_trt
  )


# Factor reformatting (e.g., Include missing in the "Unknown" category).
adsl$SEX_DECODE <- forcats::fct_na_value_to_level(
  adsl$SEX_DECODE,
  level = "Unknown"
)

adsl$AGEGR1_DECODE <- forcats::fct_na_value_to_level(
  factor(stringr::str_replace(as.character(adsl$AGEGR1), ">=", "\u2265")),
  level = "Unknown"
)

adsl$RACE_DECODE <- forcats::fct_collapse(
  forcats::fct_na_value_to_level(adsl$RACE_DECODE, level = "Unknown"),
  "Not reported or unknown" = c("Not reported", "Unknown")
)

adsl$ETHNIC_DECODE <- forcats::fct_collapse(
  forcats::fct_na_value_to_level(adsl$ETHNIC_DECODE, level = "Unknown"),
  "Not reported or unknown" = c("Not reported", "Unknown")
)

had_ae <- adae_jnj %>%
  filter(TRTEMFL == "Y") %>%
  select(USUBJID, TRTEMFL) %>%
  distinct(USUBJID, .keep_all = TRUE)

adsl <- adsl %>%
  left_join(had_ae) %>%
  mutate(TRTEMFL = ifelse(is.na(TRTEMFL), "N", "Y"))

################################################################################
# Define layout and build table
################################################################################

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)
ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)

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

extra_args_rr <- list(
  riskdiff = FALSE
)

extra_args_rr2 <- append(
  extra_args_rr,
  list(resp_var = "TRTEMFL", drop_levels = TRUE)
)

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx",
  top_level_section_div = " "
) %>%
  append_topleft("Characteristic") %>%
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) %>%
  split_cols_by(trtvar, split_fun = add_active_combo)

lyt <- lyt %>%
  analyze(
    "TRTEMFL",
    afun = a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(
        label = paste("Subjects with >= 1", subjFilterText),
        val = "Y",
        .stats = c("count_unique_fraction")
      )
    ),
    show_labels = "hidden"
  ) %>%
  analyze(
    vars = "SEX_DECODE",
    var_labels = "Sex, n/Ns (%)",
    show_labels = "visible",
    afun = a_freq_resp_var_j,
    extra_args = extra_args_rr2,
    nested = FALSE
  ) %>%
  analyze(
    vars = "AGEGR1_DECODE",
    var_labels = "Age group (years), n/Ns (%)",
    show_labels = "visible",
    afun = a_freq_resp_var_j,
    extra_args = extra_args_rr2,
    nested = FALSE
  ) %>%
  analyze(
    vars = "RACE_DECODE",
    var_labels = "Race, n/Ns (%)",
    show_labels = "visible",
    afun = a_freq_resp_var_j,
    extra_args = extra_args_rr2,
    nested = FALSE
  ) %>%
  analyze(
    vars = "ETHNIC_DECODE",
    var_labels = "Ethnicity, n/Ns (%)",
    show_labels = "visible",
    afun = a_freq_resp_var_j,
    extra_args = extra_args_rr2,
    nested = FALSE
  )

result <- build_table(lyt, adsl)

################################################################################
# Post-Processing:
# Prune any categories with all zeros:
################################################################################

result <- safe_prune_table(result, prune_func = count_pruner())

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, titles)

################################################################################
# Convert to tbl file and output table
################################################################################

tt_to_tlgrtf(result, file = fileid, orientation = "landscape")
