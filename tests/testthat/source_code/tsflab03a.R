################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsflab03a
## R version:                 4.2.1
## Short Description:         Program to create tsflab03a: Subjects With
##                            ≥1 Laboratory Values With Elevated or Low Values
##                            Based on Worst On-treatment Value Using NCI-CTCAE Criteria
##                            by [Subgroup]
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      30JAN2024
## Input:                     adsl.RDS, adlb.RDS or adlbc.RDS
## Output:                    tsflab03a.rtf
## Remarks:                   Should only be used when abnormalities are based upon lab toxicity grading
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

tblid <- "TSFLAB03a"
fileid <- write_path(opath, tblid)
titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

popfl <- "SAFFL"
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"

subgrpvar <- "AGEGR1"
subgrplbl <- "Age: %s years"

page_by <- TRUE # Set page_by TRUE/FALSE if you (do not) wish to start a new page after a new subgroup
indent_adj <- -1L
if (page_by) {
  indent_adj <- 0L
}

## if the option TRTEMFL needs to be added to the TLF
trtemfl <- FALSE

## For analysis on SI units: use adlb dataset
## For analysis on Conventional units: use adlbc dataset -- shell is in conventional units

ad_domain <- "ADLB"

################################################################################
# Initial processing of data + check if table is valid for trial:
################################################################################
adlb_complete <- adlb_jnj


## parcat5 and 6 options :

availparcat56 <- c(
  "Investigations",
  "Metabolism and nutritional disorders",
  "Renal and urinary disorders",
  "Blood and lymphatic system disorders"
)

## resrict to some
selparcat56 <- availparcat56[c(1, 2, 4)]

## get all
selparcat56 <- availparcat56

lbtoxgrade_file <- file.path(datapath, "lbtoxgrade.xlsx")
lbtoxgrade_sheets <- readxl::excel_sheets(path = lbtoxgrade_file)

### CTC5 or DAIDS21c : default CTC5

lbtoxgrade_defs <- readxl::read_excel(lbtoxgrade_file, sheet = "CTC5")

lbtoxgrade_defs <- unique(
  lbtoxgrade_defs %>%
    select(TOXTERM, TOXGRD, INDICATR)
) %>%
  mutate(
    ATOXDSCLH = TOXTERM,
    ATOXGRLH = paste("Grade", TOXGRD)
  ) %>%
  rename(ATOXDIR = INDICATR) %>%
  select(ATOXDSCLH, ATOXGRLH, ATOXDIR)


################################################################################
# Process Data:
################################################################################

adsl <- adsl_jnj %>%
  filter(.data[[popfl]] == "Y") %>%
  select(USUBJID, all_of(c(popfl, trtvar, subgrpvar)))

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
adsl$rrisk_label <- paste(adsl[[trtvar]], paste("vs", ctrl_grp))

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)


## for checking row_counts on AGEGR1 : should consist with counts from ADSL
adsl_agegr1 <- adsl %>%
  select(all_of(c(trtvar, subgrpvar))) %>%
  group_by(across(all_of(c(trtvar, subgrpvar)))) %>%
  summarise(n = n())

adlb00 <- adlb_complete %>%
  select(
    USUBJID,
    AVISITN,
    AVISIT,
    starts_with("PAR"),
    starts_with("ATOX"),
    starts_with("ANL"),
    ONTRTFL,
    TRTEMFL,
    AVAL,
    APOBLFL,
    ABLFL,
    LVOTFL
  ) %>%
  inner_join(adsl) %>%
  mutate(
    ATOXGRL = as.character(ATOXGRL),
    ATOXGRH = as.character(ATOXGRH)
  ) %>%
  relocate(
    .,
    USUBJID,
    all_of(subgrpvar),
    ANL04FL,
    ANL05FL,
    ONTRTFL,
    TRTEMFL,
    AVISIT,
    ATOXGRL,
    ATOXGRH,
    ATOXDSCL,
    ATOXDSCH,
    PARAMCD,
    AVISIT,
    AVAL,
    APOBLFL,
    ABLFL
  )


adlb00 <- adlb00 # %>%
## APT comment on PARCAT6 :
## HGB and WBC : Set to "Blood and lymphatic system disorders".
## HGB and WBC parameter are in 2 categories, one for the high and another one for the low direction grading.
## Anemia (HGB low) and Leukocytosis (WBC high) are in the category "Blood and lymphatic system disorders".
## The grading in the opposite directions are categorized under "Investigations".
## Therefor, both PARCAT5 and PARCAT6 are populated for HGB abd WBC.
## Deal with what is needed at later level, when we have splitted low and high
# mutate(PARCAT56 = coalesce(PARCAT6,PARCAT5)) %>%
# mutate(PARCAT56 = factor(PARCAT56,levels=unique(c(levels(adlb_complete$PARCAT6),levels(adlb_complete$PARCAT5)))))

# obj_label(adlb00$PARCAT56) <- "Combined PARCAT56"

### important: previous actions lost the label of variables

adlb00 <- var_relabel_list(adlb00, var_labels(adlb_complete, fill = T))

parcat <- unique(
  adlb00 %>%
    select(starts_with("PARCAT"), PARAMCD, PARAM, ATOXDSCL, ATOXDSCH) %>%
    filter(!(is.na(PARCAT5) & is.na(PARCAT6)))
)


### data preparation

if (all(selparcat56 != "")) {
  filtered_adlb <- adlb00 %>%
    filter((PARCAT5 %in% selparcat56) | (PARCAT6 %in% selparcat56))
}


### low grades : ATOXDSCL ATOXGRL ANL04FL
### Note on Worst On-treatment
### note: by filter ANL04FL/ANL05FL, this table is restricted to On-treatment values, per definition of ANL04FL/ANL05FL
### therefor, no need to add ONTRTFL in filter
### if derivation of ANL04FL/ANL05FL is not restricted to ONTRTFL records, adding ONTRTFL here will not give the correct answer either
### as mixing worst with other period is not giving the proper selection !!!

filtered_adlb_low <- filtered_adlb %>%
  filter(ANL04FL == "Y" & !is.na(ATOXDSCL) & !is.na(ATOXGRL)) %>%
  mutate(
    ATOXDSCLH = ATOXDSCL,
    ATOXGRLH = ATOXGRL,
    ATOXDIR = "LOW"
  ) %>%
  select(USUBJID, starts_with("PAR"), starts_with("ATOX"), TRTEMFL) %>%
  select(-c(ATOXGRL, ATOXGRH, ATOXDSCL, ATOXDSCH))

### high grades: ATOXDSCH ATOXGRH ANL05FL
filtered_adlb_high <- filtered_adlb %>%
  filter(ANL05FL == "Y" & !is.na(ATOXDSCH) & !is.na(ATOXGRH)) %>%
  mutate(
    ATOXDSCLH = ATOXDSCH,
    ATOXGRLH = ATOXGRH,
    ATOXDIR = "HIGH"
  ) %>%
  select(USUBJID, starts_with("PAR"), starts_with("ATOX"), TRTEMFL) %>%
  select(-c(ATOXGRL, ATOXGRH, ATOXDSCL, ATOXDSCH))

## combine Low and high into adlb_tox
filtered_adlb_tox <-
  bind_rows(
    filtered_adlb_low,
    filtered_adlb_high
  ) %>%
  select(-c(ATOXGR, ATOXGRN)) %>%
  inner_join(adsl)

### correction of proper category (PARCAT56) for HGB (LOW) and WBC (HIGH)
filtered_adlb_tox <-
  filtered_adlb_tox %>%
  mutate(
    PARCAT56 = case_when(
      PARAMCD == "HGB" & ATOXDIR == "LOW" ~ PARCAT6,
      PARAMCD == "WBC" & ATOXDIR == "HIGH" ~ PARCAT6,

      ### fix on synthetic data !!!!
      PARAMCD == "WBC" & ATOXDIR == "LOW" ~ "Investigations",
      TRUE ~ PARCAT5
    )
  ) %>%
  mutate(
    PARCAT56 = factor(
      PARCAT56,
      levels = unique(c(
        "Blood and lymphatic system disorders",
        levels(adlb_complete$PARCAT5)
      ))
    )
  )


#### DO NOT USE TRTEMFL = Y in filter, as this will remove subjects from both numerator and denominator
#### instead : set ATOXGRLH to a non-reportable value (ie Grade 0) and keep in dataset
if (trtemfl) {
  filtered_adlb_tox <- filtered_adlb_tox %>%
    mutate(
      ATOXGRLH = case_when(
        is.na(TRTEMFL) | TRTEMFL != "Y" ~ "0",
        TRUE ~ ATOXGRLH
      )
    )
}


## convert some to factors -- lty will fail if these are not factors
filtered_adlb_tox <-
  filtered_adlb_tox %>%
  mutate(
    ATOXGRLH = factor(paste("Grade", ATOXGRLH), levels = paste("Grade", 0:5)),
    ATOXDIR = factor(ATOXDIR, levels = c("LOW", "HIGH"))
  )


filtered_adlb_tox <- unique(
  filtered_adlb_tox
)

check_non_unique_subject <- filtered_adlb_tox %>%
  group_by(USUBJID, PARAMCD, ATOXDSCLH) %>%
  summarize(n_subject = n()) %>%
  filter(n_subject > 1)

if (nrow(check_non_unique_subject)) {
  message(
    "Please review your data selection process, subject has multiple records"
  )
}


params <- unique(
  filtered_adlb_tox %>% select(PARCAT56, PARAMCD, PARAM, ATOXDSCLH, ATOXDIR)
)

all_params <- unique(
  adlb_complete %>%
    filter(!(is.na(PARCAT5) & is.na(PARCAT6))) %>%
    select(PARCAT5, PARCAT6, PARAMCD, PARAM, ATOXDSCL, ATOXDSCH)
)

### add relevant extra vars to lbtoxgrade_defs, only restrict to those actually in trial
### Neutrophil Count Decreased (NEUTSG NEUT) is causing for the many-to-many warning
lbtoxgrade_defs <- lbtoxgrade_defs %>%
  inner_join(
    .,
    unique(
      filtered_adlb_tox %>%
        select(PARAMCD, PARAM, ATOXDIR, ATOXDSCLH, PARCAT5, PARCAT6, PARCAT56)
    )
  )


### Define param_map to be used in layout
param_map <- lbtoxgrade_defs %>%
  select(PARCAT56, PARAM, PARAMCD, ATOXDIR, ATOXDSCLH, ATOXGRLH) %>%
  ### for proper sorting: add factor levels to PARAMCD, ATOXDIR
  mutate(
    PARAMCD = factor(PARAMCD, levels = levels(adlb00$PARAMCD)),
    ATOXDIR = factor(ATOXDIR, levels = c("LOW", "HIGH"))
  ) %>%
  # ### actual sorting
  #   arrange(PARCAT56,PARAMCD,ATOXDIR,ATOXGRLH) %>%
  ### actual sorting -- all alphabetic on output
  arrange(PARCAT56, ATOXDSCLH) %>%
  ### !!!! no factors are allowed in this split_fun map definition
  mutate(
    PARCAT56 = as.character(PARCAT56),
    PARAMCD = as.character(PARAMCD),
    PARAM = as.character(PARAM),
    ATOXDIR = as.character(ATOXDIR),
    ATOXDSCLH = as.character(ATOXDSCLH)
  ) # %>%
### !!!! do not remove Grade 0 here, as this would lead to incorrect N and % derivation
### filter(ATOXGRLH != "Grade 0")
### Grade 0 will be removed as a post-processing step

################################################################################
# Define layout and build table:
################################################################################

extra_args_rr <- list(
  method = "wald",
  denom = "n_df",
  ref_path = ref_path,
  .stats = c("denom", "count_unique_fraction"),
  denom_by = subgrpvar
)
extra_args_rr2 <- list(
  method = "wald",
  denom = "n_df",
  ref_path = ref_path,
  .stats = c("denom", "count_unique_denom_fraction"),
  denom_by = subgrpvar
)


lyt0 <- basic_table(show_colcounts = TRUE, colcount_format = "N=xx") %>%
  ### first columns
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) %>%
  split_cols_by(trtvar) %>%
  split_cols_by("rrisk_header", nested = FALSE) %>%
  split_cols_by(
    trtvar,
    labels_var = "rrisk_label",
    split_fun = remove_split_levels(ctrl_grp)
  ) %>%
  split_rows_by(
    subgrpvar,
    label_pos = "hidden",
    section_div = " ",
    split_fun = drop_split_levels,
    page_by = page_by
  ) %>%
  ###
  summarize_row_groups(
    var = subgrpvar,
    cfun = a_freq_j,
    extra_args = list(
      label_fstr = subgrplbl,
      denom = "n_altdf",
      denom_by = subgrpvar,
      riskdiff = FALSE,
      extrablankline = TRUE,
      .stats = c("n_altdf")
    )
  ) %>%
  split_rows_by(
    "PARCAT56",
    label_pos = "topleft",
    child_labels = "visible",
    split_label = "NCI-CTCAE Category",
    ### trim_levels_to_map needs to be applied at ALL split_rows_by levels
    split_fun = trim_levels_to_map(param_map),
    section_div = " ",
    indent_mod = indent_adj,
  ) %>%
  split_rows_by(
    "ATOXDSCLH",
    label_pos = "topleft",
    child_labels = "visible",
    split_label = "Laboratory Test",
    ### trim_levels_to_map needs to be applied at ALL split_rows_by levels
    split_fun = trim_levels_to_map(param_map),
    section_div = " "
  ) %>%
  append_topleft("    Grade, n (%)")

# version without explicit denominator (as in shell)
lyt <- lyt0 %>%
  # for testing, it is sometimes convenient to explicitely show the used denominator
  analyze(
    "ATOXGRLH",
    a_freq_j,
    extra_args = extra_args_rr,
    show_labels = "hidden",
    indent_mod = 0L
  )

result <- build_table(lyt, filtered_adlb_tox, alt_counts_df = adsl)

# version with explicit denominator (for verification)
lyt2 <- lyt0 %>%
  # for testing, it is sometimes convenient to explicitely show the used denominator
  analyze(
    "ATOXGRLH",
    a_freq_j,
    extra_args = extra_args_rr2,
    show_labels = "visible",
    indent_mod = 0L
  )

### apply layout
result2 <- build_table(lyt2, filtered_adlb_tox, alt_counts_df = adsl)

################################################################################
# Post-Processing:
# - Remove Grade 0 line
# - Remove colcount from rrisk_header
################################################################################

remove_grade0 <- function(tr) {
  if (is(tr, "DataRow") & (tr@label == "Grade 0")) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

result <- result %>% prune_table(prune_func = keep_rows(remove_grade0))

################################################################################
# Remove colcount from rrisk_header:
################################################################################

result <- remove_col_count(result)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, titles)

################################################################################
# Convert to tbl file and output table
################################################################################

tt_to_tlgrtf(result, file = fileid, orientation = "landscape")
