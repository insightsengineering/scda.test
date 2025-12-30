################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsids02a
## R version:                 4.2.1
## Short Description:         Program to create tsids02a: Subject Disposition by subgroup
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      05JAN2024
## Input:                     adsl.R
## Output:                    tsids02a.rtf
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
# Define output ID and file location:
################################################################################

tblid <- "TSIDS02a"
fileid <- write_path(opath, tblid)
popfl <- "FASFL"
trtvar <- "TRT01P"
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)


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
    RACE_DECODE
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

# Added since label_fstr not working with cpct_relrisk
adsl$RACE <- as.factor(as.character(ifelse(
  is.na(adsl$RACE_DECODE),
  NA,
  paste("Race:", adsl$RACE_DECODE)
)))

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = "Placebo",
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)
ref_path <- c("colspan_trt", " ", trtvar, "Placebo")

################################################################################
# Define layout and build table:
################################################################################

totdf <- tribble(
  ~valname, ~label, ~levelcombo, ~exargs,
  "Total", "Total", c("Xanomeline High Dose", "Xanomeline Low Dose", "Placebo"), list()
)


extra_args1 <- list(
  denom = "n_altdf",
  denom_by = "RACE",
  riskdiff = FALSE,
  .stats = "count_unique"
)
extra_args2 <- list(
  denom = "n_altdf",
  denom_by = "RACE",
  riskdiff = FALSE,
  .stats = "count_unique_fraction"
)
extra_args3 <- list(
  denom = "n_altdf",
  denom_by = "RACE",
  riskdiff = TRUE,
  method = "wald",
  .stats = "count_unique_fraction",
  ref_path = ref_path
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
  split_rows_by("RACE", split_fun = drop_split_levels, section_div = " ") %>%
  summarize_row_groups(
    "RACE",
    cfun = a_freq_j,
    indent_mod = 0L,
    # na_str = " ",
    extra_args = extra_args1
  ) %>%
  # Analysis sets
  analyze(
    popfl,
    var_labels = "Analysis set:",
    afun = a_freq_j,
    extra_args = append(extra_args2, list(label = "Full", val = "Y")),
    show_labels = "visible"
  ) %>%
  analyze(
    "SAFFL",
    afun = a_freq_j,
    extra_args = append(extra_args2, list(label = "Safety", val = "Y")),
    show_labels = "hidden",
    indent_mod = 1
  ) %>%
  analyze(
    "PPROTFL",
    afun = a_freq_j,
    extra_args = append(
      extra_args2,
      list(label = "Per protocol", val = "Y", extrablankline = TRUE)
    ),
    show_labels = "hidden",
    indent_mod = 1,
    na_str = " "
  ) %>%
  #  Ongoing
  analyze(
    "EOSSTT",
    show_labels = "hidden",
    afun = a_freq_j,
    na_str = " ",
    extra_args = append(
      extra_args2,
      list(val = "ONGOING", label = "Subjects ongoing", extrablankline = TRUE)
    )
  ) %>%
  # Treatment disposition
  analyze(
    "EOTSTT",
    table_names = "Compl_Trt",
    show_labels = "hidden",
    afun = a_freq_j,
    extra_args = append(
      extra_args3,
      list(label = "Completed treatment", val = "COMPLETED")
    )
  ) %>%
  analyze(
    "EOTSTT",
    table_names = "DC_Trt",
    show_labels = "hidden",
    afun = a_freq_j,
    extra_args = append(
      extra_args3,
      list(label = "Discontinued treatment", val = "DISCONTINUED")
    )
  ) %>%
  analyze(
    "DCTREAS",
    show_labels = "hidden",
    indent_mod = 1,
    afun = a_freq_j,
    extra_args = append(
      extra_args3,
      list(extrablankline = TRUE, drop_levels = TRUE)
    )
  ) %>%
  # Study disposition
  analyze(
    "EOSSTT",
    table_names = "Compl_Study",
    show_labels = "hidden",
    afun = a_freq_j,
    extra_args = append(
      extra_args3,
      list(label = "Completed study", val = "COMPLETED")
    )
  ) %>%
  analyze(
    "EOSSTT",
    show_labels = "hidden",
    table_names = "DC_Study",
    afun = a_freq_j,
    extra_args = append(
      extra_args3,
      list(label = "Discontinued study", val = "DISCONTINUED")
    )
  ) %>%
  analyze(
    "DCSREAS",
    show_labels = "hidden",
    indent_mod = 1,
    afun = a_freq_j,
    extra_args = append(extra_args3, list(drop_levels = TRUE))
  )

result <- build_table(lyt, adsl, alt_counts_df = adsl)

################################################################################
# Post-Processing
################################################################################

# Remove the N=xx column headers for the risk difference columns
result <- remove_col_count(result, span_label_var = "rrisk_header")

# Sort DCTREAS and DCSREAD by descending total column.
result <- result %>%
  sort_at_path(
    path = c("RACE", "*", "DCTREAS"),
    scorefun = jj_complex_scorefun(colpath = "Total", lastcat = "Other")
  ) %>%
  sort_at_path(
    path = c("RACE", "*", "DCSREAS"),
    scorefun = jj_complex_scorefun(colpath = "Total")
  )

result <- prune_table(
  result,
  prune_func = remove_rows(removerowtext = "No data to report")
)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table:
################################################################################

tt_to_tlgrtf(result, file = fileid, orientation = "landscape")
