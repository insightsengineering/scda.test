################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsfvit06
## R version:                 4.2.1
## Short Description:         Program to create tsfvit06: Subjects With
##                            Treatment-emergent Orthostatic Hypotension During
##                            [Treatment Period]
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      30JAN2024
## Input:                     adsl.RDS, advs.RDS
## Output:                    tsfvit06.rtf
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
library(dplyr)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSFVIT06"
fileid <- write_path(opath, tblid)
titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

popfl <- "SAFFL"
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"

# specify in the order you want to print on table
selparamcd <- c("ORTHYP", "DIABPO", "SYSBPO")

selparamcdN <- tibble(PARAMCD = selparamcd, PARAMCDN = seq_along(selparamcd))

### Per email June 12: DAS/SDS confirmed to NOT restrict to on-treatment values

################################################################################
# Process Data:
################################################################################

adsl <- adsl_jnj %>%
  filter(.data[[popfl]] == "Y") %>%
  select(USUBJID, all_of(c(popfl, trtvar)), SEX, AGEGR1, RACE, ETHNIC)


adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)


colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)
ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)

### N is the number of subjects with postbaseline orthostatic measurements and without orthostatic hypotension at baseline
## do not use TRTEMFL as filter, as this will only select AVALC = Y records per definition of TRTEMFL
## instead : start from post-baseline records and retain one record per subject
## for those subjects with both Y and N records, keep the Y record
## for those subjects with only Y records or only N records, keep one Y, N record respectively
filtered_advs <- advs_jnj %>%
  filter(PARAMCD %in% selparamcd & APOBLFL == "Y") %>%
  filter(PARAMCD %in% selparamcd) %>%
  mutate(
    AVALC = case_when(
      (PARAMCD == "SYSBPO" | PARAMCD == "DIABPO") & CRIT1FL == "Y" ~ "Y",
      (PARAMCD == "SYSBPO" | PARAMCD == "DIABPO") &
        (CRIT1FL == "N" |
          CRIT1FL == NA) ~ "N",
      PARAMCD == "ORTHYP" ~ AVALC
    ),
    PARAM = case_when(
      PARAMCD == "SYSBPO" ~ "SBP (STD-SUP) <-20",
      PARAMCD == "DIABPO" ~ "DBP (STD-SUP) <-10",
      PARAMCD == "ORTHYP" ~ PARAM
    )
  ) %>%
  select(
    STUDYID,
    USUBJID,
    PARAMCD,
    PARAM,
    AVALC,
    AVISIT,
    APOBLFL,
    TRTEMFL,
    ONTRTFL
  ) %>%
  inner_join(adsl) %>%
  ### ensure to keep only 1 result per subject, keep N only in case no Y was observed
  arrange(USUBJID, PARAMCD, AVALC) %>%
  group_by(USUBJID, PARAMCD) %>%
  mutate(navalc = n_distinct(AVALC)) %>%
  filter(!(navalc > 1 & AVALC == "N")) %>%
  ## only keep one record
  slice_head(n = 1) %>%
  ungroup()

#### remove subjects abnormal for "ORTHYP" at baseline
bl_abn_orthyp <- advs_jnj %>%
  filter(PARAMCD == "ORTHYP" & ABLFL == "Y" & AVALC == "Y")

### actually remove the subjects with AVALC = Y for ORTHYP
### N is the number of subjects with postbaseline orthostatic measurements and without orthostatic hypotension at baseline
filtered_advs <- filtered_advs %>%
  filter(!(USUBJID %in% unique(bl_abn_orthyp$USUBJID)))

### get sorting as per order in selparamcdN
selparamcdN <- selparamcdN %>%
  left_join(unique(filtered_advs %>% select(PARAMCD, PARAM))) %>%
  arrange(PARAMCDN)

param_levels <- unique(as.character(selparamcdN$PARAM))

filtered_advs$PARAM <- factor(
  as.character(filtered_advs$PARAM),
  levels = param_levels
)


### Mapping for AVALC
### alternative approach to retrieve from metadata iso dataset
xlabel_map <- unique(filtered_advs %>% select(PARAM, PARAMCD)) %>%
  mutate(
    value = "Y",
    label = as.character(PARAM)
  ) %>%
  mutate(
    label = case_when(
      label ==
        "Orthostatic Hypotension" ~ "Total number of subjects with orthostatic hypotension",
      TRUE ~ label
    )
  )

################################################################################
# Define layout and build table:
################################################################################

extra_args_rr <- list(
  method = "wald",
  denom = "n_df",
  ref_path = ref_path,
  .stats = c("count_unique_fraction")
)

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) %>%
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) %>%
  split_cols_by(trtvar) %>%
  analyze(
    "AVALC",
    a_freq_j,
    show_labels = "hidden",
    table_names = "AVALC_N",
    extra_args = list(denom = "n_df", .stats = c("n_df"))
  ) %>%
  split_rows_by(
    "PARAM",
    label_pos = "topleft",
    split_fun = drop_split_levels,
    child_labels = "hidden",
    split_label = "Orthostatic hypotension, n (%)"
  ) %>%
  # as in shell, do not show denom in count/denom (%)
  ### indent will be fixed to 1, will be updated later in post-processing
  analyze(
    "AVALC",
    a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(
        val = c("Y"),
        label_map = xlabel_map
      )
    ),
    indent_mod = 2L,
    show_labels = "hidden"
  )

result <- build_table(lyt, filtered_advs, alt_counts_df = adsl)

################################################################################
# Post-Processing:
# - update indent for "SYSBPO" and "DIABPO"
# - remove unwanted colcounts
################################################################################

## update indent for "SYSBPO" and "DIABPO"

adj_indent_mod <- function(result, path, indentupd) {
  indent_mod(tt_at_path(result, path)) <- indent_mod(tt_at_path(result, path)) +
    indentupd
  return(result)
}

result <- adj_indent_mod(
  result,
  path = c("PARAM", "Orthostatic Hypotension"),
  indentupd = -1L
)

result <- remove_col_count(result)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, titles)

################################################################################
# Convert to tbl file and output table
################################################################################

tt_to_tlgrtf(result, file = fileid)
