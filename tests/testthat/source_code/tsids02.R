################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsids02
## R version:                 4.2.1
## Short Description:         Program to create tsids02: Subject Disposition
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      05JAN2024
## Input:                     adsl.R
## Output:                    tsids02.rtf
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

tblid <- "TSIDS02"
fileid <- write_path(opath, tblid)
popfl <- "FASFL"
trtvar <- "TRT01P"
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


################################################################################
# Process data:
################################################################################

adsl <- adsl_jnj

no_data_to_report <- function(df, var) {
  if (sum(is.na(df[[var]])) == length(df[[var]])) {
    df[[var]] <- factor(NA_character_, levels = "No data to report")
  }
  return(df)
}

adsl <- no_data_to_report(df = adsl, var = "DCTREAS")
adsl <- no_data_to_report(df = adsl, var = "DCSREAS")

adsl <- adsl %>%
  filter(!!rlang::sym(popfl) == "Y") %>%
  select(
    USUBJID,
    !!rlang::sym(trtvar),
    !!rlang::sym(popfl),
    SAFFL,
    PPROTFL,
    EOTSTT,
    DCTREAS,
    EOSSTT,
    DCSREAS,
    RACE
  ) %>%
  create_colspan_var(
    non_active_grp = "Placebo",
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = trtvar
  ) %>%
  mutate(
    rrisk_header = "Risk Difference (%) 95% CI",
    rrisk_label = paste(!!rlang::sym(trtvar), "vs Placebo")
  )

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

totdf <- tribble(
  ~valname , ~label  , ~levelcombo                                                 , ~exargs ,
  "Total"  , "Total" , c("Xanomeline High Dose", "Xanomeline Low Dose", "Placebo") , list()
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
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) %>%
  split_cols_by(trtvar) %>%
  split_cols_by(
    trtvar,
    split_fun = add_combo_levels(totdf, keep_levels = "Total"),
    nested = FALSE
  ) %>%
  split_cols_by("rrisk_header", nested = FALSE) %>%
  split_cols_by(
    trtvar,
    labels_var = "rrisk_label",
    split_fun = remove_split_levels("Placebo")
  ) %>%
  # Analysis sets
  analyze(
    popfl,
    var_labels = "Analysis set",
    afun = a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(label = "Full", val = "Y", riskdiff = FALSE, NULL)
    ),
    show_labels = "visible"
  ) %>%
  analyze(
    "SAFFL",
    afun = a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(label = "Safety", val = "Y", riskdiff = FALSE, NULL)
    ),
    show_labels = "hidden",
    indent_mod = 1
  ) %>%
  analyze(
    "PPROTFL",
    afun = a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(
        label = "Per protocol",
        val = "Y",
        riskdiff = FALSE,
        extrablankline = TRUE,
        NULL
      )
    ),
    show_labels = "hidden",
    indent_mod = 1,
    na_str = " "
  ) %>%
  # Ongoing
  analyze(
    "EOSSTT",
    show_labels = "hidden",
    afun = a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(
        label = "Subjects ongoing",
        val = "ONGOING",
        riskdiff = FALSE,
        extrablankline = TRUE,
        NULL
      )
    ),
    na_str = " "
  ) %>%
  # Treatment disposition
  analyze(
    "EOTSTT",
    table_names = "Compl_Trt",
    show_labels = "hidden",
    afun = a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(label = "Completed treatment", val = "COMPLETED", NULL)
    )
  ) %>%
  analyze(
    "EOTSTT",
    table_names = "DC_Trt",
    show_labels = "hidden",
    afun = a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(label = "Discontinued treatment", val = "DISCONTINUED", NULL)
    )
  ) %>%
  analyze(
    "DCTREAS",
    show_labels = "hidden",
    indent_mod = 1,
    afun = a_freq_j,
    na_str = " ",
    extra_args = append(extra_args_rr, list(extrablankline = TRUE))
  ) %>%
  # Study disposition
  analyze(
    "EOSSTT",
    table_names = "Compl_Study",
    show_labels = "hidden",
    afun = a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(label = "Completed study", val = "COMPLETED", NULL)
    )
  ) %>%
  analyze(
    "EOSSTT",
    show_labels = "hidden",
    table_names = "DC_Study",
    afun = a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(label = "Discontinued study", val = "DISCONTINUED", NULL)
    )
  ) %>%
  analyze(
    "DCSREAS",
    show_labels = "hidden",
    indent_mod = 1,
    afun = a_freq_j,
    extra_args = append(extra_args_rr, NULL)
  )

result <- build_table(lyt, adsl)

################################################################################
# Post-Processing
################################################################################

## Remove the N=xx column headers for the risk difference columns
result <- remove_col_count(result, span_label_var = "rrisk_header")

result <- result %>%
  sort_at_path(
    path = c(
      "ma_FASFL_SAFFL_PPROTFL_EOSSTT_Compl_Trt_DC_Trt_DCTREAS_Compl_Study_DC_Study_DCSREAS",
      "DCTREAS"
    ),
    scorefun = jj_complex_scorefun(colpath = "Total", lastcat = "Other")
  ) %>%
  sort_at_path(
    path = c(
      "ma_FASFL_SAFFL_PPROTFL_EOSSTT_Compl_Trt_DC_Trt_DCTREAS_Compl_Study_DC_Study_DCSREAS",
      "DCSREAS"
    ),
    scorefun = jj_complex_scorefun(colpath = "Total", lastcat = "Other")
  )

# Prune data driven output.
result <- result %>%
  safe_prune_table(prune_func = keep_rows(keep_non_null_rows)) %>%
  safe_prune_table(
    prune_func = count_pruner(
      cols = c("colspan_trt"),
      cat_exclude = c(
        "Completed study",
        "Completed treatment",
        "Discontinued study",
        "Discontinued treatment"
      )
    )
  )

# Prune data driven output.
result <- result %>%
  safe_prune_table(prune_func = keep_rows(keep_non_null_rows)) %>%
  safe_prune_table(
    prune_func = count_pruner(
      cols = c("colspan_trt"),
      cat_exclude = c(
        "Completed study",
        "Completed treatment",
        "Discontinued study",
        "Discontinued treatment"
      )
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
