################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsfae07a.R
## R version:                 4.2.1
## junco Version:             1.0
## Short Description:         Program to create tsfae07a: AE table of special interest - severity version
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      27 Nov 2023
## Input:                     ADSL, ADAE, ADLB (optional)
## Output:                    TSFAE07a.rtf
## Remarks:                   Template R script version using rtables framework
##
## Modification History:
##  Rev #:                    1
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

################################################################################
# - Define output ID and file location
# - Define treatment variable used (default=TRT01A)
# - Define population flag used (default=SAFFL)
# - Define the customized query variable (CQzzNAM) for the AEs of special interest
# - Define the ADaM Variable to choose the AE related variable(s) and Action taken variable(s)
# - Choose whether or not you want to present a combined active treatment column (default=TRUE)
# - Choose whether or not you want to present the risk difference columns (default=TRUE)
# - Choose which risk difference method you would like (default=Wald)
# - Define what the control treatment group is for your study (e.g Placebo)
# - Define how to create combined treatment columns (if required)
# - Define lab parameters that are required for the optional section of the table
################################################################################

tblid <- "TSFAE07a"
fileid <- write_path(opath, tblid)
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)


trtvar <- "TRT01A"
popfl <- "SAFFL"

special_interest_var <- "CQ01NAM"
aerelvar <- "AEREL"
aeactionvar <- "AEACN"

combined_colspan_trt <- TRUE
risk_diff <- TRUE
rr_method <- "wald"
ctrl_grp <- "Placebo"

if (combined_colspan_trt == TRUE) {
  # Set up levels and label for the required combined columns
  add_combo <- add_combo_facet(
    "Combined",
    label = "Combined",
    levels = c("Xanomeline High Dose", "Xanomeline Low Dose")
  )

  # choose if any facets need to be removed - e.g remove the combined column for placebo
  rm_combo_from_placebo <- cond_rm_facets(
    facets = "Combined",
    ancestor_pos = NA,
    value = " ",
    split = "colspan_trt"
  )

  mysplit <- make_split_fun(post = list(add_combo, rm_combo_from_placebo))
}

# Optional lab section parameters
labsection <- TRUE
lab_params <- c("AST", "ALT")
lab_labels <- c("AST>5xULN", "ALT>5xULN")
lab_var <- c("MCRIT1MN", "MCRIT1MN")
lab_vals <- c("2,3", "2,3") # Note both character or numeric values can be enclosed within in one set of double quotations here

################################################################################
# Process Data:
################################################################################

adsl <- adsl_jnj %>%
  filter(!!rlang::sym(popfl) == "Y") %>%
  select(STUDYID, USUBJID, all_of(trtvar), all_of(popfl))

lbdata <- NULL

# Optional lab section
if (labsection == TRUE) {
  for (i in 1:length(lab_params)) {
    filter_val <- unlist(strsplit(lab_vals[[i]], ","))

    adlb <- adlb_jnj %>%
      filter(
        TRTEMFL == "Y" &
          PARAMCD == lab_params[[i]] &
          !!rlang::sym(lab_var[[i]]) %in% filter_val
      ) %>%
      mutate(criteria = lab_labels[[i]]) %>%
      select(USUBJID, TRTEMFL, PARAMCD, all_of(lab_var[[i]]), criteria)

    lbdata <- bind_rows(adlb, lbdata)
  }

  adlb <- lbdata %>%
    group_by(USUBJID, TRTEMFL, PARAMCD, criteria) %>%
    slice(1) %>%
    ungroup()

  # Create flag variable for each parameter that met the condition to merge back onto adsl
  # Overall row
  adlbparamall <- adlb %>%
    filter(PARAMCD %in% lab_params) %>%
    mutate(lab_flag = "Y") %>%
    group_by(USUBJID) %>%
    slice(1) %>%
    ungroup() %>%
    select(USUBJID, lab_flag)

  adsl <- left_join(adsl, adlbparamall, by = c("USUBJID"))

  # Loop through each parameter that the user has specified and merge flag variable onto adsl
  for (i in 1:length(lab_params)) {
    flagvar <- paste0("lab_flag", i)

    adlbparam <- adlb %>%
      filter(PARAMCD == lab_params[[i]] & criteria == lab_labels[[i]]) %>%
      mutate(!!flagvar := lab_labels[[i]]) %>%
      group_by(USUBJID) %>%
      slice(1) %>%
      ungroup() %>%
      select(USUBJID, all_of(flagvar))
    adsl <- left_join(adsl, adlbparam, by = c("USUBJID"))
  }
}

adae <- adae_jnj %>%
  filter(TRTEMFL == "Y" & !is.na(!!rlang::sym(special_interest_var))) %>%
  select(
    USUBJID,
    TRTEMFL,
    AEBODSYS,
    AEDECOD,
    AESEV,
    all_of(special_interest_var),
    AESER,
    AEOUT,
    all_of(aeactionvar),
    all_of(aerelvar)
  )

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == "Placebo", " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

if (risk_diff == TRUE) {
  adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
  adsl$rrisk_label <- paste(adsl[[trtvar]], paste("vs", ctrl_grp))
}

# join data together
ae <- adae %>% right_join(., adsl, by = c("USUBJID"))

# Keep only maximum severity for the particular AESI
ae <- ae %>%
  mutate(
    ASEV = factor(
      ifelse(is.na(AESEV), "Missing", as.character(AESEV)),
      levels = c("Severe", "Moderate", "Mild", "Missing")
    ),
    rowhead = "Maximum severity"
  ) %>%
  arrange(USUBJID, special_interest_var, ASEV) %>%
  group_by(USUBJID, .data[[special_interest_var]]) %>%
  slice(1) %>%
  ungroup()

# Remove Missing as a level since not required in table
levels(ae$ASEV)[levels(ae$ASEV) == "Missing"] <- NA

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

ref_path <- c("colspan_trt", " ", "TRT01A", "Placebo")
extra_args_rr <- list(
  method = rr_method,
  ref_path = ref_path,
  .stats = c("count_unique_fraction")
)

lyt <- basic_table(
  top_level_section_div = " ",
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) %>%
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  )

if (combined_colspan_trt == TRUE) {
  lyt <- lyt %>%
    split_cols_by(trtvar, split_fun = mysplit)
} else {
  lyt <- lyt %>%
    split_cols_by(trtvar)
}

if (risk_diff == TRUE) {
  lyt <- lyt %>%
    split_cols_by("rrisk_header", nested = FALSE) %>%
    split_cols_by(
      trtvar,
      labels_var = "rrisk_label",
      split_fun = remove_split_levels("Placebo")
    )
}

lyt <- lyt %>%
  split_rows_by(
    special_interest_var,
    split_label = "",
    split_fun = trim_levels_in_group("AEDECOD"),
    label_pos = "topleft",
    indent_mod = 0,
    section_div = c(" ")
  ) %>%
  summarize_row_groups(
    special_interest_var,
    cfun = a_freq_j,
    extra_args = append(extra_args_rr, NULL)
  ) %>%
  analyze(
    "AEDECOD",
    var_labels = "Preferred Term",
    afun = a_freq_j,
    indent_mod = 0,
    show_labels = "visible",
    extra_args = append(extra_args_rr, NULL)
  ) %>%
  split_rows_by(
    "rowhead",
    split_label = "",
    # ,split_fun = trim_levels_in_group("ASEV")
    label_pos = "topleft",
    indent_mod = 0,
    section_div = c(" ")
  ) %>%
  analyze(
    "ASEV",
    afun = a_freq_j,
    indent_mod = 0,
    extra_args = append(extra_args_rr, NULL)
  ) %>%
  split_rows_by(
    "AESER",
    split_fun = keep_split_levels("Y"),
    section_div = c(" ")
  ) %>%
  summarize_row_groups(
    "AESER",
    cfun = a_freq_j,
    extra_args = append(extra_args_rr, list(label = "Serious", NULL))
  ) %>%
  analyze(
    "AEOUT",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = append(
      extra_args_rr,
      list(label = "Deaths", val = "FATAL", NULL)
    )
  ) %>%
  analyze(
    aeactionvar,
    afun = a_freq_j,
    show_labels = "hidden",
    nested = FALSE,
    extra_args = append(
      extra_args_rr,
      list(
        label = "Resulting in treatment discontinuation",
        val = "DRUG WITHDRAWN",
        NULL
      )
    )
  ) %>%
  analyze(
    aerelvar,
    afun = a_freq_j,
    show_labels = "hidden",
    nested = FALSE,
    extra_args = append(
      extra_args_rr,
      list(label = "Related~[super a]", val = "RELATED", NULL)
    )
  )

if (labsection == TRUE) {
  lyt <- lyt %>%
    analyze(
      "lab_flag",
      afun = a_freq_j,
      show_labels = "hidden",
      nested = FALSE,
      extra_args = append(
        extra_args_rr,
        list(label = "Laboratory assessment~[super b]", NULL)
      )
    )

  for (i in 1:length(lab_params)) {
    aflagvar <- paste0("lab_flag", i)

    lyt <- lyt %>%
      analyze(
        aflagvar,
        afun = a_freq_j,
        indent_mod = 1,
        show_labels = "hidden",
        nested = TRUE,
        extra_args = append(extra_args_rr, NULL)
      )
  }
}

lyt <- lyt %>%
  append_topleft("AESI Assessment, n (%)")

result <- build_table(lyt, ae, alt_counts_df = adsl)

## Remove the N=xx column headers for the risk difference columns
result <- remove_col_count(result)

## Remove any rogue null rows
result <- result %>%
  safe_prune_table(prune_func = keep_rows(keep_non_null_rows))

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table
################################################################################

tt_to_tlgrtf(result, file = fileid, orientation = "landscape")
