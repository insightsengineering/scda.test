################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsfecg05
## R version:                 4.2.1
## Short Description:         Program to create tsfecg05: Subjects With ECG Values
##                            Outside Specified Limits Based on
##                            On-treatment Value and Over Time
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      30JAN2024
## Input:                     adsl.RDS, adeg.RDS
## Output:                    tsfecg05.rtf
## Remarks:                   carefully review factor levels of variables CRIT1/CRIT2 on your input adeg.rds dataset
##                            check consistency in order CRIT1 and CRIT2 : here: CRIT2 (low), CRIT1 (high)
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

tblid <- "TSFECG05"
fileid <- write_path(opath, tblid)
titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

popfl <- "SAFFL"
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"

ad_domain <- "adeg"

selvisit <- c(
  "Month 1",
  "Month 3",
  "Month 6",
  "Month 9",
  "Month 12",
  "Month 15",
  "Month 18",
  "Month 24"
)

selvisit <- c("Month 1", "Month 3")

catvar <- c("CRIT1", "CRIT2")

################################################################################
# initial read of data
################################################################################

adeg_complete <- adeg_jnj

## check parameters that have CRIT1,CRIT2 defined in data
levels_data <- unique(adeg_complete %>% select(PARAMCD, PARAM, CRIT1, CRIT2))

# restrict to these
selparamcd <- as.character(unique(
  levels_data %>% filter(!(is.na(CRIT1) | is.na(CRIT2))) %>% pull(PARAMCD)
))

## if the option TRTEMFL needs to be added to the TLF
trtemfl <- TRUE

################################################################################
# Mapping for CRIT1/2
################################################################################

xlabel_map <- levels_data %>%
  tidyr::pivot_longer(
    cols = c("CRIT1", "CRIT2"),
    names_to = "var",
    values_to = "label"
  ) %>%
  filter(!is.na(label)) %>%
  mutate(
    label = as.character(label),
    var = paste0(var, "FL"),
    value = "Y"
  )

#### Note: this is not in line with the markedly abnormal file
## for both EGHRMN & PRAG: crit1 and crit2 are reversed
## crit1 and crit2 on data are consistent with the order specified in the shell : low/high

################################################################################
# Process Data:
################################################################################

adsl <- adsl_jnj %>%
  filter(.data[[popfl]] == "Y") %>%
  select(USUBJID, all_of(c(trtvar, popfl)))

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
adsl$rrisk_label <- paste(adsl[[trtvar]], paste("vs", ctrl_grp))


adeg <- adeg_complete %>%
  filter(PARAMCD %in% selparamcd) %>%
  select(
    USUBJID,
    PARAM,
    PARAMCD,
    AVISIT,
    AVAL,
    CRIT1,
    CRIT1FL,
    CRIT2,
    CRIT2FL,
    starts_with("ANL") & ends_with("FL"),
    ONTRTFL,
    TRTEMFL,
    APOBLFL
  ) %>%
  inner_join(., adsl)


################################################################################
##### filter data
################################################################################

### alert on trtemfl, do not apply it as a filter, as this would lead to incorrect denominators

adeg_crit_any <- unique(
  adeg %>%
    # synthetic data is missing the ANL04FL variable, however, this wouldn't be sufficient either, unless it flags both CRIT1 and CRIT2
    filter(ONTRTFL == "Y") %>%
    mutate(AVISIT = factor("On-treatment")) %>%
    select(-c(AVAL, ANL01FL, ANL02FL, ANL03FL))
)

### note: this does not necesarily leads to one record per parameter per subject
### if ANL04FL is available, we still can end up with more than one record
### a subject can have crit1fl=Y at one visit, and crit2fl=Y at another visit, and crit1fl=N&crit2fl=N at another
### as long as the analysis function deals with multiple records per subject correctly, this is not an issue

check_dup_sub <- adeg_crit_any %>%
  group_by(USUBJID, PARAMCD, AVISIT) %>%
  mutate(n_rec = n()) %>%
  filter(n_rec > 1)

# Over time is also restricted to on treatment value
adeg_crit_overtime <- adeg %>%
  filter(ANL02FL == "Y" & AVISIT %in% selvisit & ONTRTFL == "Y") %>%
  select(-c(AVAL, ANL01FL, ANL02FL, ANL03FL))

adeg_crit_comb <- rbind(adeg_crit_any, adeg_crit_overtime) %>%
  mutate(
    AVISIT = factor(
      as.character(AVISIT),
      levels = c("On-treatment", levels(adeg_crit_overtime$AVISIT))
    )
  ) %>%
  inner_join(., adsl)


#### DO NOT USE TRTEMFL = Y in filter, as this will remove subjects from both numerator and denominator
#### instead : set "CRIT2FL","CRIT1FL" to a non-reportable value (ie N) and keep in dataset
if (trtemfl) {
  adeg_crit_comb <- adeg_crit_comb %>%
    mutate(
      CRIT1FL = case_when(
        is.na(TRTEMFL) | TRTEMFL != "Y" ~ "N",
        TRUE ~ CRIT1FL
      ),
      CRIT2FL = case_when(
        is.na(TRTEMFL) | TRTEMFL != "Y" ~ "N",
        TRUE ~ CRIT2FL
      )
    )
}

adeg_crit_comb <- adeg_crit_comb %>%
  mutate(
    CRIT1FL = factor(CRIT1FL, levels = c("Y", "N")),
    CRIT2FL = factor(CRIT2FL, levels = c("Y", "N"))
  )

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

extra_args_rr1 <- list(method = "wald", denom = "n_df", .stats = c("n_df"))
extra_args_rr2 <- list(
  method = "wald",
  denom = "n_df",
  .stats = c("count_unique_fraction")
)

lyt <- basic_table(show_colcounts = TRUE, colcount_format = "N=xx") %>%
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) %>%
  split_cols_by(
    trtvar
    # , split_fun = add_combo_levels(combodf)
  ) %>%
  # re-enable the below if relative risk columns are wanted
  # split_cols_by("rrisk_header", nested = FALSE) %>%
  # split_cols_by(trtvar, labels_var = "rrisk_label",
  #               split_fun = remove_split_levels(ctrl_grp)) %>%
  #
  split_rows_by(
    "PARAM",
    split_label = "Parameter",
    label_pos = "topleft",
    split_fun = drop_split_levels
  ) %>%
  split_rows_by(
    "AVISIT",
    split_label = "Study Visit",
    label_pos = "topleft",
    section_div = " ",
    split_fun = drop_split_levels
  ) %>%
  analyze(
    c("CRIT1"),
    a_freq_j,
    extra_args = extra_args_rr1,
    show_labels = "hidden",
    indent_mod = 0L
  ) %>%
  # denominators are varying per test, no need to show as N is shown in line above
  # revise order to first present low then high
  analyze(
    c("CRIT2FL", "CRIT1FL"),
    a_freq_j,
    extra_args = append(
      extra_args_rr2,
      list(
        val = c("Y"),
        label_map = xlabel_map
      )
    ),
    show_labels = "hidden",
    indent_mod = 1L
  ) %>%
  append_topleft("    Criteria, n (%)")

result <- build_table(lyt, adeg_crit_comb, alt_counts_df = adsl)


################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, titles)

################################################################################
# Convert to tbl file and output table
################################################################################

tt_to_tlgrtf(result, file = fileid)
