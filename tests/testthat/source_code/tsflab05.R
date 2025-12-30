################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsflab05
## R version:                 4.2.1
## Short Description:         Program to create tsflab05:
##                            Subjects With [Last/Any] On-treatment
##                            Hematology Values [= Level 2] Criteria
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      30JAN2024
## Input:                     adsl.RDS, adlb.RDS or adlbc.RDS
## Output:                    tsflab05.rtf
## Remarks:                   Should only be used when abnormalities are based upon lab toxicity file
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

tblid <- "TSFLAB05"
titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)
fileid <- write_path(opath, tblid)

popfl <- "SAFFL"
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"

grade_threshold <- "2"

## For toxicity grades, the units should not matter, work with adlb dataset as default

ad_domain <- "ADLB"

#### table options:
# last_any <- "LAST"
last_any <- "ANY"
### if ANY, then Subjects with Any on-treatment value >= Level 2 will be presented (ANL04FL/ANL05FL/ONTRTFL will be used here)
### if Last, then Subjects with Last on-treatment value >= Level 2 will be presented (LVOTFL will be used here)

## if the option TRTEMFL needs to be added to the TLF -- ensure the same setting as in tsflabxxxx
trtemfl <- TRUE

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

################################################################################
# Initial processing of data
################################################################################

adlb_complete <- adlb_jnj



################################################################################
# Process lab toxicity file
################################################################################

lbtoxgrade_file <- file.path(datapath, "lbtoxgrade.xlsx")
lbtoxgrade_sheets <- readxl::excel_sheets(path = lbtoxgrade_file)

### CTC5 or DAIDS21c : default CTC5

lbvars <- c("LBTESTCD", "LBTEST", "LBSPEC", "LBMETHOD")

lbtoxgrade_defs <- readxl::read_excel(lbtoxgrade_file, sheet = "CTC5")

lbtoxgrade_defs <- unique(
  unique(
    lbtoxgrade_defs %>%
      select(all_of(lbvars), TOXTERM, TOXGRD, INDICATR)
  ) %>%
    mutate(
      ATOXDSCLH = TOXTERM,
      ATOXGRLH = paste("Grade", TOXGRD)
    ) %>%
    rename(ATOXDIR = INDICATR) %>%
    select(all_of(lbvars), ATOXDSCLH, ATOXDIR)
)


################################################################################
# Initial processing of data
################################################################################

### be aware, there are toxicity terms that are based upon diff tests (example "Neutrophil Count Decreased": NEUT and NEUTSG) !!!!!
### if both tests are included in ADaM dataset, review your derivations carefully
attention_terms <-
  unique(lbtoxgrade_defs %>% select(ATOXDSCLH, all_of(lbvars))) %>%
  group_by(ATOXDSCLH) %>%
  mutate(n = n_distinct(LBTESTCD)) %>%
  filter(n > 1)

ad_toxterms <- bind_rows(
  unique(
    adlb_complete %>%
      select(PARAMCD, ATOXDSCL) %>%
      mutate(TOXTERM = ATOXDSCL, TOXDIR = "LOW")
  ),
  unique(
    adlb_complete %>%
      select(PARAMCD, ATOXDSCH) %>%
      mutate(TOXTERM = ATOXDSCH, TOXDIR = "HIGH")
  )
) %>%
  select(PARAMCD, TOXTERM, TOXDIR) %>%
  filter(!is.na(TOXTERM)) %>%
  arrange(TOXTERM, TOXDIR, PARAMCD)


attention_ad_toxterms <- ad_toxterms %>%
  group_by(TOXTERM) %>%
  mutate(n = n_distinct(PARAMCD)) %>%
  filter(n > 1)

#### From here onwards: avoid using PARAMCD, to ensure toxterm is combined

### could also work with param_lookup and lbtoxgrade_defs
### ALERT: do not include PARAMCD here as some toxicity terms are based on more than one PARAMCD: here : NEUT and NEUTSG both have TOXTERM = Neutrophil Count Decreased
toxterms <- unique(
  adlb_complete %>%
    filter(!(is.na(ATOXDSCL) & is.na(ATOXDSCH))) %>%
    ### do not include PARAMCD here!!!!
    select(ATOXDSCL, ATOXDSCH) %>%
    tidyr::pivot_longer(
      .,
      cols = c("ATOXDSCL", "ATOXDSCH"),
      names_to = "VARNAME",
      values_to = "ATOXDSCLH"
    )
) %>%
  filter(!is.na(ATOXDSCLH))

### convert dataframe into label_map that can be used with the a_freq_j afun function
xlabel_map <- toxterms %>%
  mutate(
    var = "ATOXGRLHx",
    value = "Y",
    label = as.character(ATOXDSCLH)
  ) %>%
  select(ATOXDSCLH, value, label)

################################################################################
# Process Data:
################################################################################

adsl <- adsl_jnj %>%
  filter(.data[[popfl]] == "Y") %>%
  select(STUDYID, USUBJID, all_of(c(popfl, trtvar)))

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

flagvars <- c("ONTRTFL", "TRTEMFL", "LVOTFL")
adlb00 <- adlb_complete %>%
  select(
    USUBJID,
    AVISITN,
    AVISIT,
    starts_with("PAR"),
    starts_with("ATOX"),
    starts_with("ANL"),
    all_of(flagvars),
    LBSEQ,
    AVAL,
    AVALC
  ) %>%
  inner_join(adsl) %>%
  mutate(
    ATOXGRL = as.character(ATOXGRL),
    ATOXGRH = as.character(ATOXGRH)
  ) %>%
  relocate(
    .,
    USUBJID,
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
    AVAL
  )


# adlb00 <- adlb00 #%>%
## APT comment on PARCAT6 :
## HGB and WBC : Set to "Blood and lymphatic system disorders".
## HGB and WBC parameter are in 2 categories, one for the high and another one for the low direction grading.
## Anemia (HGB low) and Leukocytosis (WBC high) are in the category "Blood and lymphatic system disorders".
## The grading in the opposite directions are categorized under "Investigations".
## Therefor, both PARCAT5 and PARCAT6 are populated for HGB abd WBC.
## Deal with what is needed at later level, when we have splitted low and high

# obj_label(adlb00$PARCAT56) <- "Combined PARCAT56"

### important: previous actions lost the label of variables

adlb00 <- var_relabel_list(adlb00, var_labels(adlb_complete, fill = T))

if (all(selparcat56 != "")) {
  filtered_adlb <- adlb00 %>%
    filter((PARCAT5 %in% selparcat56) | (PARCAT6 %in% selparcat56))
} else {
  filtered_adlb <- adlb00
}

### low grades : ATOXDSCL ATOXGRL ANL04FL
filtered_adlb_low <- filtered_adlb %>%
  filter(!is.na(ATOXDSCL) & !is.na(ATOXGRL)) %>%
  mutate(
    ATOXDSCLH = ATOXDSCL,
    ATOXGRLH = ATOXGRL,
    ATOXDIR = "LOW",
    ANL045FL = ANL04FL
  ) %>%
  select(
    USUBJID,
    starts_with("PAR"),
    starts_with("ATOX"),
    all_of(flagvars),
    ANL04FL,
    ANL05FL,
    ANL045FL,
    LBSEQ,
    AVAL,
    AVALC
  ) %>%
  select(-c(ATOXGRL, ATOXGRH, ATOXDSCL, ATOXDSCH))

### high grades: ATOXDSCH ATOXGRH ANL05FL
filtered_adlb_high <- filtered_adlb %>%
  filter(ANL05FL == "Y" & !is.na(ATOXDSCH) & !is.na(ATOXGRH)) %>%
  mutate(
    ATOXDSCLH = ATOXDSCH,
    ATOXGRLH = ATOXGRH,
    ATOXDIR = "HIGH",
    ANL045FL = ANL05FL
  ) %>%
  select(
    USUBJID,
    starts_with("PAR"),
    starts_with("ATOX"),
    all_of(flagvars),
    ANL04FL,
    ANL05FL,
    ANL045FL,
    LBSEQ,
    AVAL,
    AVALC
  ) %>%
  select(-c(ATOXGRL, ATOXGRH, ATOXDSCL, ATOXDSCH))


## combine Low and high into adlb_tox
filtered_adlb_tox <-
  bind_rows(
    filtered_adlb_low,
    filtered_adlb_high
  ) %>%
  select(-c(ATOXGR, ATOXGRN)) %>%
  mutate(ATOXGRLHN = as.numeric(ATOXGRLH)) %>%
  inner_join(adsl)


### correction of proper category (PARCAT56) for HGB (LOW) and WBC (HIGH)
filtered_adlb_tox <-
  filtered_adlb_tox %>%
  mutate(
    PARCAT56 = case_when(
      PARAMCD == "HGB" & ATOXDIR == "LOW" ~ PARCAT6,
      PARAMCD == "WBC" & ATOXDIR == "HIGH" ~ PARCAT6,
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


filtered_adlb_tox <- unique(
  filtered_adlb_tox
)

filtered_adlb_tox_1 <- filtered_adlb_tox # %>%
# ## On treatment
# filter(ONTRTFL == "Y")

### Note on On-treatment
### note: by filter ANL04FL/ANL05FL, this table is restricted to On-treatment values, per definition of ANL04FL/ANL05FL
### Same for LVOTFL
### therefor, no need to add ONTRTFL in filter
### if derivation of ANL04FL/ANL05FL/LVOTFL is not restricted to ONTRTFL records, adding ONTRTFL here will not give the correct answer either
### as mixing worst with other period is not giving the proper selection !!!

if (toupper(last_any) == "ANY") {
  filtered_adlb_tox_1 <- filtered_adlb_tox_1 %>%
    ## Optional : Any : ensure to have one record per subject for direction
    filter(ANL04FL == "Y" | ANL05FL == "Y")
}

if (toupper(last_any) == "LAST") {
  filtered_adlb_tox_1 <- filtered_adlb_tox_1 %>%
    ## Optional : last on treatment record only
    filter(LVOTFL == "Y")
}


#### DO NOT USE TRTEMFL = Y in filter, as this will remove subjects from both numerator and denominator
#### instead : set ATOXGRLH to a non-reportable value (ie Grade 0) and keep in dataset
if (trtemfl) {
  filtered_adlb_tox_1 <- filtered_adlb_tox_1 %>%
    mutate(
      ATOXGRLHN = case_when(
        is.na(TRTEMFL) | TRTEMFL != "Y" ~ 0,
        TRUE ~ ATOXGRLHN
      )
    )
}


### Alphabetical sorting of toxicity terms
atoxdsclh_levels <- sort(as.character(unique(filtered_adlb_tox_1$ATOXDSCLH)))

filtered_adlb_tox_1 <- filtered_adlb_tox_1 %>%
  mutate(
    ATOXGRLHx = case_when(
      ATOXGRLHN >= as.numeric(grade_threshold) ~ "Y",
      TRUE ~ "N"
    )
  ) %>%
  mutate(ATOXGRLHx = factor(ATOXGRLHx, levels = c("Y", "N"))) %>%
  mutate(ATOXDSCLH = factor(ATOXDSCLH, levels = atoxdsclh_levels))


# check uniqueness
check_non_unique_subject <- filtered_adlb_tox_1 %>%
  group_by(USUBJID, ATOXDSCLH) %>%
  mutate(n_subject = n()) %>%
  filter(n_subject > 1)

if (nrow(check_non_unique_subject)) {
  message(
    "Please review your data selection process, subject has multiple records"
  )
}

### syntethic data: no records for NEUT/NEUTSG for the further selection (ATOXGRL is never populated for these records)
xx <- adlb00 %>%
  filter(ATOXDSCL == "Neutrophil Count Decreased" & !is.na(ATOXGRL))


################################################################################
# Define layout and build table:
################################################################################

extra_args_rr <- list(
  method = "wald",
  denom = "n_df",
  .stats = c("count_unique_denom_fraction"),
  ref_path = ref_path
)


lyt <- basic_table(show_colcounts = TRUE, colcount_format = "N=xx") %>%
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
    "PARCAT56",
    label_pos = "topleft",
    child_labels = "visible",
    split_label = "Laboratory Category",
    split_fun = drop_split_levels,
    section_div = " "
  ) %>%
  split_rows_by(
    "ATOXDSCLH",
    label_pos = "topleft",
    split_label = paste0(
      "Laboratory Test \u2265 Grade ",
      grade_threshold,
      ", n (%)"
    ),
    child_labels = "hidden",
    split_fun = drop_split_levels
  ) %>%
  analyze(
    "ATOXGRLHx",
    afun = a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(
        val = c("Y"),
        label_map = xlabel_map
      )
    ),
    show_labels = "hidden",
    indent_mod = 0L
  )

result <- build_table(lyt, filtered_adlb_tox_1, alt_counts_df = adsl)

################################################################################
# Post-Processing:
# - Remove colcount from rrisk_header
################################################################################

### Issue: tests with only 1 direction (either low or high) get a line with label a_freq_j (analyze function)
### remove these lines here

result <- result %>% prune_table(prune_func = keep_rows(keep_non_null_rows))

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
