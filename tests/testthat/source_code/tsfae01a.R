################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsfae01a
## R version:                 4.2.1
## Short Description:         Program to create tsfae01a: Overall Summary of
##                            Adverse Events
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      30JAN2024
## Input:                     adsl.RDS, adae.RDS
## Output:                    tsfae01a.rtf
## Remarks:                   This variant includes summary of AEs by max severity.
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

tblid <- "TSFAE01a"
fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01A"
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


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
  mutate(
    rrisk_header = "Risk Difference (%) (95% CI)",
    rrisk_label = paste(!!rlang::sym(trtvar), "vs Placebo")
  ) %>%
  select(
    USUBJID,
    !!rlang::sym(popfl),
    !!rlang::sym(trtvar),
    colspan_trt,
    rrisk_header,
    rrisk_label
  )

adae <- adae_jnj %>%
  filter(TRTEMFL == "Y") %>%
  select(
    USUBJID,
    AESER,
    AESDTH,
    AESLIFE,
    AESHOSP,
    AESDISAB,
    AESCONG,
    AESMIE,
    AEACN_DECODE,
    AESEV
  ) %>%
  group_by(USUBJID) %>%
  mutate(maxsev = max(as.character(AESEV), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(maxsev = ifelse(is.na(maxsev), "Missing", maxsev)) %>%
  mutate(
    maxsev = factor(maxsev, levels = c("Mild", "Moderate", "Severe", "Missing"))
  )

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

##################################
# Check the levels of AEACN_DECODE
##################################

aeacn_levels <- levels(adae$AEACN_DECODE)
# Here we are not considering "Drug Withdrawn", "Dose Not Changed", "Not Applicable"
excl_aeacn_levels <- c("Drug Withdrawn", "Dose Not Changed", "Not Applicable")
dosemod_lvls <- aeacn_levels[!(aeacn_levels %in% excl_aeacn_levels)]


## rearrange levels for AEACN_DECODE

newsort_AEACN_DECODE <- unique(c(
  "Drug Interrupted",
  "Dose Reduced",
  "Dose Rate Reduced",
  "Dose Increased",
  "Unknown",
  aeacn_levels
))

adae$AEACN_DECODE <- forcats::fct_relevel(
  adae$AEACN_DECODE,
  newsort_AEACN_DECODE
)

## mapping table for label updates

dosemod_lblmap <- tibble(value = dosemod_lvls, label = dosemod_lvls) %>%
  mutate(
    label = case_when(
      value == "Dose Increased" ~ label,
      value == "Dose Reduced" ~ "Reduction of study treatment",
      value == "Drug Interrupted" ~ "Interruption of study treatment",
      TRUE ~ label
    )
  )


dosemod_spf <- make_combo_splitfun(
  nm = "modified",
  label = "AE leading to dose modification of study",
  levels = c(
    "Dose Reduced",
    "Dose Increased",
    "Drug Interrupted",
    "Dose Rate Reduced",
    "Unknown"
  )
)
aesevall_spf <- make_combo_splitfun(
  nm = "AESEV_ALL",
  label = "Any AE~[super a]",
  levels = NULL
)


rr_method <- "wald"
ref_path <- c("colspan_trt", " ", trtvar, "Placebo")
extra_args_rr <- list(
  method = rr_method,
  ref_path = ref_path,
  .stats = c("count_unique_fraction")
)

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx",
  top_level_section_div = " "
) %>%
  append_topleft(c(" ", " ", "Event, n (%)")) %>%
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) %>%
  split_cols_by(trtvar) %>%
  split_cols_by("rrisk_header", nested = FALSE) %>%
  split_cols_by(
    trtvar,
    labels_var = "rrisk_label",
    split_fun = remove_split_levels("Placebo")
  ) %>%
  split_rows_by(
    "AESER",
    split_fun = keep_split_levels("Y"),
    section_div = " "
  ) %>%
  summarize_row_groups(
    "AESER",
    cfun = a_freq_j,
    extra_args = list(
      label = "SAE",
      method = rr_method,
      ref_path = ref_path,
      .stats = c("count_unique_fraction")
    )
  ) %>%
  analyze(
    "AESDTH",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = append(
      extra_args_rr,
      list(label = "With fatal outcome", val = "Y", NULL)
    )
  ) %>%
  analyze(
    "AESLIFE",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = append(
      extra_args_rr,
      list(label = "Life-threatening", val = "Y", NULL)
    )
  ) %>%
  analyze(
    "AESHOSP",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = append(
      extra_args_rr,
      list(label = "Requiring or prolonging hospitalization", val = "Y", NULL)
    )
  ) %>%
  analyze(
    "AESDISAB",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = append(
      extra_args_rr,
      list(
        label = "Resulting in persistent or significant disability/incapacity",
        val = "Y",
        NULL
      )
    )
  ) %>%
  analyze(
    "AESCONG",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = append(
      extra_args_rr,
      list(label = "Congenital anomaly or birth defect", val = "Y", NULL)
    )
  ) %>%
  analyze(
    "AESMIE",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = append(extra_args_rr, list(label = "Other", val = "Y", NULL))
  ) %>%
  analyze(
    "AEACN_DECODE",
    afun = a_freq_j,
    nested = FALSE,
    extra_args = append(
      extra_args_rr,
      list(
        label = "AE leading to permanent discontinuation of study treatment",
        val = "Drug Withdrawn",
        NULL
      )
    )
  ) %>%
  split_rows_by("AEACN_DECODE", split_fun = dosemod_spf, section_div = " ") %>%
  summarize_row_groups(
    "AEACN_DECODE",
    cfun = a_freq_j,
    extra_args = list(
      label = "AE leading to dose modification of study treatment",
      method = rr_method,
      ref_path = ref_path,
      .stats = c("count_unique_fraction")
    )
  ) %>%
  analyze(
    "AEACN_DECODE",
    table_names = "AEACN_DECODE",
    a_freq_j,
    show_labels = "hidden",
    extra_args = append(
      extra_args_rr,
      list(
        excl_levels = excl_aeacn_levels,
        label_map = dosemod_lblmap,
        drop_levels = TRUE
      )
    )
  ) %>%
  split_rows_by("maxsev", split_fun = aesevall_spf) %>%
  summarize_row_groups(
    "maxsev",
    cfun = a_freq_j,
    extra_args = list(
      label = "Any AE~[super a]",
      method = rr_method,
      ref_path = ref_path,
      .stats = c("count_unique_fraction")
    )
  ) %>%
  analyze("maxsev", afun = a_freq_j, extra_args = append(extra_args_rr, NULL))


result <- build_table(lyt, adae, alt_counts_df = adsl)

################################################################################
# Post-Processing:
# - Remove N's from Risk cols
# - Prune any categories with all zeros.
################################################################################

result <- remove_col_count(result)
result <- suppressWarnings(safe_prune_table(
  result,
  prune_func = count_pruner(
    cat_exclude = c(
      "With fatal outcome",
      "Life-threatening",
      "Requiring or prolonging hospitalization",
      "Resulting in persistent or significant disability/incapacity",
      "Congenital anomaly or birth defect",
      "Other"
    )
  )
))

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table:
################################################################################

tt_to_tlgrtf(result, file = fileid, orientation = "landscape")
