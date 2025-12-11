################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsflab04b
## R version:                 4.2.1
## Short Description:         Program to create tsflab04b:
##                            Subjects With [Last/Any] On-treatment
##                            Hematology Values [= Level 2] Criteria
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      30JAN2024
## Input:                     adsl.RDS, adlb.RDS or adlbc.RDS
## Output:                    tsflab04b.rtf
## Remarks:                   Should only be used when abnormalities are based upon markedly abnormal file
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
is_template_pgm <- TRUE #### !!!! set to FALSE for actual study!!
test_stop <- FALSE #### !!!! set to FALSE for actual study!!

tblid <- "TSFLAB04b"
fileid <- write_path(opath, tblid)
titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

popfl <- "SAFFL"
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"

level_threshold <- "2"

## For analysis on SI units: use adlb dataset
## For analysis on Conventional units: use adlbc dataset -- shell is in conventional units

ad_domain <- "ADLB"

#### table options:
# last_any <- "LAST"
last_any <- "ANY"
### if ANY, then Subjects with Any on-treatment value >= Level 2 will be presented (ANL04FL/ANL05FL/ONTRTFL will be used here)
### if Last, then Subjects with Last on-treatment value >= Level 2 will be presented (LVOTFL will be used here)

## if the option TRTEMFL needs to be added to the TLF -- ensure the same setting as in tsflab02
trtemfl <- TRUE

################################################################################
# Initial processing of data + check if table is valid for trial:
################################################################################
adlb_complete <- adlb_jnj


################################################################################
# Retrieve markedly abnormal values from spreadsheet:
################################################################################
### Markedly Abnormal spreadsheet

markedlyabnormal_file <- file.path(datapath, "markedlyabnormal.xlsx")




lbmarkedlyabnormal_defs <- readxl::read_excel(
  markedlyabnormal_file,
  sheet = toupper(ad_domain)
) %>%
  filter(PARAMCD != "Parameter Code")

MCRITs <- unique(
  lbmarkedlyabnormal_defs %>%
    filter(!stringr::str_ends(VARNAME, "ML")) %>%
    pull(VARNAME)
)


MCRITs_def <- unique(
  lbmarkedlyabnormal_defs %>%
    filter(VARNAME %in% MCRITs) %>%
    select(PARAMCD, VARNAME, CRIT, SEX)
) %>%
  mutate(VARNAME = paste0(VARNAME, "ML")) %>%
  rename(CRITNAME = CRIT)


MCRITs_def2 <- lbmarkedlyabnormal_defs %>%
  filter(VARNAME %in% paste0(MCRITs, "ML"))


MCRITs_def3 <- MCRITs_def2 %>%
  left_join(., MCRITs_def, relationship = "many-to-one") %>%
  mutate(
    CRITx = paste0("(", stringr::str_split_i(CRIT, stringr::fixed("("), 2))
  ) %>%
  mutate(CRITx = paste(CRITNAME, CRITx)) %>%
  select(PARAMCD, SEX, VARNAME, CRITx, ORDER) %>%
  mutate(VARNAME = paste0(VARNAME, "x")) %>%
  mutate(CRITn = as.character(4 - as.numeric(ORDER)))


### the threshold value will be used for the label, eg if level 2 (also level 3 will be reported as met level 2)
### the label on the output will be the label from the chosen threshold
MCRITs_def4 <- MCRITs_def3 %>%
  filter(CRITn == level_threshold)


### convert dataframe into label_map that can be used with the a_freq_j afun function
xlabel_map <- MCRITs_def4 %>%
  rename(var = VARNAME, label = CRITx) %>%
  mutate(
    var = stringr::str_replace(var, "MLx", "MNx"),
    value = "Y"
  ) %>%
  select(PARAMCD, var, value, label)


#### extend xlabel_map to include both MCRIT1 and MCRIT2 for all parameters
params <- unique(xlabel_map %>% select(PARAMCD, var, value)) %>%
  tidyr::expand(PARAMCD, var, value)

xlabel_map <- params %>%
  left_join(., xlabel_map) %>%
  mutate(
    label = case_when(
      is.na(label) ~ "TO BE DELETED",
      TRUE ~ label
    )
  )

### for fixing a problem with one test : HDL, it has MCRIT1 male low, MCRIT2 female low
### our data only has MCRIT1
### get text of label prior to "(", to process later
xlabel_map$label_short <- stringr::str_split_i(
  xlabel_map$label,
  stringr::fixed("("),
  1
)


#### extend xlabel_map to include both MCRIT1 and MCRIT2 for all parameters
params <- unique(xlabel_map %>% select(PARAMCD, var, value)) %>%
  tidyr::expand(PARAMCD, var, value)

xlabel_map <- params %>%
  left_join(., xlabel_map) %>%
  mutate(
    label = case_when(
      is.na(label) ~ "TO BE DELETED",
      TRUE ~ label
    )
  )

### for fixing a problem with one test : HDL, it has MCRIT1 male low, MCRIT2 female low
### our data only has MCRIT1
### get text of label prior to "(", to process later
xlabel_map$label_short <- stringr::str_split_i(
  xlabel_map$label,
  stringr::fixed("("),
  1
)

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

adlb_complete <- adlb_jnj

adlb00 <- adlb_complete %>%
  select(
    USUBJID,
    AVISITN,
    AVISIT,
    starts_with("PAR"),
    starts_with("ANL"),
    starts_with("MCRIT"),
    ONTRTFL,
    TRTEMFL,
    LVOTFL,
    AVAL
  ) %>%
  inner_join(adsl) %>%
  relocate(
    USUBJID,
    AVISITN,
    AVISIT,
    PARAMCD,
    MCRIT1ML,
    MCRIT2ML,
    ANL04FL,
    ANL05FL,
    ONTRTFL,
    TRTEMFL,
    LVOTFL
  )


### previous actions on adlb lost the label of variables

adlb00 <- var_relabel_list(adlb00, var_labels(adlb_complete, fill = T))

### data preparation

filtered_adlb_0 <- adlb00 %>%
  filter(PARAMCD %in% unique(MCRITs_def3$PARAMCD)) %>%
  filter(toupper(PARCAT1) == "HEMATOLOGY") %>%
  select(
    USUBJID,
    AVISITN,
    AVISIT,
    PARAMCD,
    PARAM,
    PARCAT1,
    PARCAT2,
    PARCAT3,
    ONTRTFL,
    TRTEMFL,
    LVOTFL,
    MCRIT1ML,
    MCRIT1MN,
    MCRIT1,
    MCRIT2ML,
    MCRIT2MN,
    MCRIT2,
    ANL04FL,
    ANL05FL
  ) %>%
  inner_join(adsl)


filtered_adlb_1 <- filtered_adlb_0 # %>%
# ## On treatment
# filter(ONTRTFL == "Y")

### Note on On-treatment
### note: by filter ANL04FL/ANL05FL, this table is restricted to On-treatment values, per definition of ANL04FL/ANL05FL
### Same for LVOTFL
### therefor, no need to add ONTRTFL in filter
### if derivation of ANL04FL/ANL05FL/LVOTFL is not restricted to ONTRTFL records, adding ONTRTFL here will not give the correct answer either
### as mixing worst with other period is not giving the proper selection !!!

if (toupper(last_any) == "ANY") {
  filtered_adlb_1 <- filtered_adlb_1 %>%
    ## Optional : Any : ensure to have one record per subject for direction
    filter(ANL04FL == "Y" | ANL05FL == "Y")
}

if (toupper(last_any) == "LAST") {
  filtered_adlb_1 <- filtered_adlb_1 %>%
    ## Optional : last on treatment record only
    filter(LVOTFL == "Y")
}

#### DO NOT USE TRTEMFL = Y in filter, as this will remove subjects from both numerator and denominator
#### instead : set MCRIT1MN to a non-reportable value (ie Level 0) and keep in dataset
if (trtemfl) {
  filtered_adlb_1 <- filtered_adlb_1 %>%
    mutate(
      MCRIT1MN = case_when(
        !is.na(MCRIT1MN) & is.na(TRTEMFL) | TRTEMFL != "Y" ~ 0,
        TRUE ~ MCRIT1MN
      ),
      MCRIT2MN = case_when(
        !is.na(MCRIT2MN) & is.na(TRTEMFL) | TRTEMFL != "Y" ~ 0,
        TRUE ~ MCRIT2MN
      ),
      MCRIT1ML = case_when(
        !is.na(MCRIT1ML) & is.na(TRTEMFL) | TRTEMFL != "Y" ~ "Level 0",
        TRUE ~ MCRIT1ML
      ),
      MCRIT2ML = case_when(
        !is.na(MCRIT2ML) & is.na(TRTEMFL) | TRTEMFL != "Y" ~ "Level 0",
        TRUE ~ MCRIT2ML
      ),
    )
}


filtered_adlb_1 <- filtered_adlb_1 %>%
  mutate(
    MCRIT1MLx = case_when(
      !is.na(MCRIT1ML) ~ paste0(
        "(",
        stringr::str_split_i(MCRIT1ML, stringr::fixed("("), 2)
      ),
      TRUE ~ NA
    )
  ) %>%
  mutate(
    MCRIT1MLx = case_when(
      !is.na(MCRIT1ML) ~ paste(MCRIT1, MCRIT1MLx),
      TRUE ~ NA
    )
  ) %>%
  mutate(
    MCRIT2MLx = case_when(
      !is.na(MCRIT2ML) ~ paste0(
        "(",
        stringr::str_split_i(MCRIT2ML, stringr::fixed("("), 2)
      ),
      TRUE ~ NA
    )
  ) %>%
  mutate(
    MCRIT2MLx = case_when(
      !is.na(MCRIT2ML) ~ paste(MCRIT2, MCRIT2MLx),
      TRUE ~ NA
    )
  ) %>%
  relocate(
    USUBJID,
    AVISITN,
    AVISIT,
    PARAM,
    PARAMCD,
    MCRIT1MLx,
    MCRIT2MLx,
    MCRIT1ML,
    MCRIT2ML,
    ANL04FL,
    ANL05FL,
    ONTRTFL,
    TRTEMFL,
    LVOTFL
  )


filtered_adlb_1 <- filtered_adlb_1 %>%
  mutate(
    MCRIT1MNx = case_when(
      MCRIT1MN >= as.numeric(level_threshold) ~ "Y",
      !is.na(MCRIT1) ~ "N"
    ),
    MCRIT2MNx = case_when(
      MCRIT2MN >= as.numeric(level_threshold) ~ "Y",
      !is.na(MCRIT2) ~ "N"
    )
  ) %>%
  mutate(
    MCRIT1MNx = factor(MCRIT1MNx, levels = c("Y", "N")),
    MCRIT2MNx = factor(MCRIT2MNx, levels = c("Y", "N"))
  )


paramcd_param <- unique(filtered_adlb_1 %>% select(PARAMCD, PARAM))


xlabel_map <- xlabel_map %>%
  right_join(., paramcd_param) %>%
  arrange(PARAM, var)

### to resolve a problem with HDL -- get from the data what MCRIT1 and MCRIT2 are being used
xlabel_map_data <- unique(
  filtered_adlb_1 %>%
    select(PARAMCD, PARAM, MCRIT1, MCRIT2) %>%
    tidyr::pivot_longer(cols = (c("MCRIT1", "MCRIT2")))
) %>%
  mutate(
    label_short_data = case_when(
      is.na(value) ~ "TO BE DELETED",
      TRUE ~ value
    ),
    var = paste0(name, "MNx")
  ) %>%
  select(PARAMCD, PARAM, var, label_short_data)

## actual fix for HDL female
xlabel_map <- xlabel_map %>%
  right_join(., xlabel_map_data) %>%
  arrange(PARAM, var) %>%
  ## actual fix for not including HDL female if not derived on data
  mutate(
    label = case_when(
      label_short_data == "TO BE DELETED" &
        label != "TO BE DELETED" ~ "TO BE DELETED",
      TRUE ~ label
    )
  )


################################################################################
# Define layout and build table:
################################################################################

extra_args_rr2 <- list(
  denom = "n_df",
  method = "wald",
  ref_path = ref_path,
  .stats = c("count_unique_denom_fraction")
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
    "PARCAT3",
    label_pos = "topleft",
    child_labels = "visible",
    split_label = "Laboratory Category",
    split_fun = drop_split_levels,
    section_div = " "
  ) %>%
  split_rows_by(
    "PARAM",
    label_pos = "topleft",
    child_labels = "hidden",
    split_label = paste0(
      "Laboratory Test >= Level ",
      level_threshold,
      ", n (%)"
    ),
    split_fun = drop_split_levels
  ) %>%
  # denominators are varying per test, therefor show denom (not yet in shell)
  analyze(
    c("MCRIT1MNx", "MCRIT2MNx"),
    afun = a_freq_j,
    extra_args = append(
      extra_args_rr2,
      list(
        val = c("Y"),
        label_map = xlabel_map
      )
    ),
    show_labels = "hidden",
    indent_mod = 0L
  )

result <- build_table(lyt, filtered_adlb_1, alt_counts_df = adsl)

################################################################################
# Post-Processing:
# - Remove colcount from rrisk_header
################################################################################

### Issue: tests with only 1 direction (either low or high) get a line with label a_freq_j (analyze function)
### remove these lines here, as well as lines with label TO BE DELETED

result <- result %>%
  prune_table(prune_func = keep_rows(keep_non_null_rows)) %>%
  prune_table(prune_func = remove_rows(removerowtext = "TO BE DELETED"))

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
