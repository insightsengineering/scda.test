################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsimh01
## R version:                 4.2.1
## Short Description:         Program to create tsimh01: [Medical History/Medical History of Interest] by
##                            System Organ Class and Preferred Term
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      30JAN2024
## Input:                     adsl.RDS, mh.sas7bdat
## Output:                    tsimh01.rtf
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

tblid <- "TSIMH01"
fileid <- write_path(opath, tblid)
titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)
popfls <- c("FASFL", "SAFFL")
popfl <- popfls[1]
trtvar <- "TRT01P"
ctrl_grp <- "Placebo"

table_vars <- c("MHOCCUR", "MHCAT", "MHBODSYS", "MHDECOD", "MHENRTPT", "MHTERM")

################################################################################
# Process data:
################################################################################

adsl <- adsl_jnj %>%
  filter(.data[[popfl]] == "Y") %>%
  select(STUDYID, USUBJID, all_of(popfls), starts_with("TRT01"))

mh <- mh_jnj

if (!("MHOCCUR" %in% names(mh))) {
  ## add missing MHOCCUR variable for now
  mh$MHOCCUR <- "Y"
}

mh <- mh %>% select(all_of(c("USUBJID", table_vars)))

## define formats - most could be removed if we could start from metadata-enhanced rds dataset

mh <- mh %>%
  mutate(
    MHDECOD = factor(case_when(
      MHDECOD == "" ~ paste0("Uncoded: ", MHTERM),
      .default = MHDECOD
    )),
    MHBODSYS = factor(case_when(
      MHBODSYS == "" ~ "Uncoded",
      .default = MHBODSYS
    ))
  )

### restrict to mh
### here : use MHENRTPT for ONGOING

mh <- mh %>%
  filter(MHENRTPT == "ONGOING")

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)


mh <- mh %>% inner_join(., adsl, by = c("USUBJID"))

## update label genmh_label to match selection that has been made
genmh_label <- "Subjects with ≥1 medical history"

genmh_label <- "Subjects with ≥1 ongoing medical history"

## genmh_label <-  "Subjects with ≥1 medical history of interest"  # when a different selection is made

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

################################################################################
# Define layout and build table:
################################################################################

split_fun <- drop_split_levels

countfraction_subjects <- function(df, labelstr, .var, .N_col, id = "USUBJID") {
  nsub <- length(unique(df[[id]]))
  denom <- .N_col
  count_fraction <- c(nsub, nsub / denom)

  ret <- in_rows(
    count_fraction,
    .formats = jjcsformat_count_fraction,
    .labels = sprintf(labelstr)
  )

  return(ret)
}

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
  add_overall_col("Total") %>%
  analyze(
    "MHOCCUR",
    var_labels = "Total number of cycles received, n (%)",
    afun = a_freq_j,
    extra_args = list(
      label = genmh_label,
      .stats = c("count_unique_fraction")
    ),
    show_labels = "hidden"
  ) %>%
  split_rows_by(
    "MHBODSYS",
    split_label = "System Organ Class",
    split_fun = trim_levels_in_group("MHDECOD"),
    label_pos = "topleft",
    section_div = c(" ")
  ) %>%
  ### ensure to utilize a cfun that counts unique subjects
  summarize_row_groups("MHBODSYS", cfun = countfraction_subjects) %>%
  count_occurrences(
    "MHDECOD",
    .stats = c("count_fraction"),
    .formats = c("count_fraction" = jjcsformat_count_fraction)
  ) %>%
  append_topleft("  Preferred Term, n (%)")

result <- build_table(lyt, mh, alt_counts_df = adsl)

################################################################################
# Post-Processing:
# - sort by descending count on risk diff column if it exists
# or active treatment columns if it does not
################################################################################

result <- sort_at_path(
  result,
  c("root", "MHBODSYS"),
  scorefun = jj_complex_scorefun("Total")
)
result <- sort_at_path(
  result,
  c("root", "MHBODSYS", "*", "MHDECOD"),
  scorefun = jj_complex_scorefun("Total")
)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, titles)

################################################################################
# Convert to tbl file and output table:
################################################################################

tt_to_tlgrtf(result, fileid)
