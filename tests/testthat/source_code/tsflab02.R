################################################################################
# Prep Environment
################################################################################

library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(tidytlg)
library(grid)
library(stringr)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSFLAB02"
fileid <- write_path(opath, tblid)
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

# Population flag variable (default=SAFFL).
popfl <- "SAFFL"
# Actual treatment variable (default=TRT01A).
trtvar <- "TRT01A"

ctrl_grp <- "Placebo"

# Add Active Study Agent Combined column
combined_colspan_trt <- TRUE

if (combined_colspan_trt == TRUE) {
  add_combo <- add_combo_facet(
    "Combined",
    label = "Combined",
    levels = c("Xanomeline High Dose", "Xanomeline Low Dose")
  )

  rm_combo_from_placebo <- cond_rm_facets(
    facets = "Combined",
    ancestor_pos = NA,
    value = " ",
    split = "colspan_trt"
  )

  mysplit <- make_split_fun(post = list(add_combo, rm_combo_from_placebo))
}

# PARCAT1 categories to produce: CHEMISTRY -> CHM, HEMATOLOGY -> HM
parcat1_categories <- list(
  chm = "CHEMISTRY",
  hem = "HEMATOLOGY"
)

ad_domain <- "adlb"

## if the option TRTEMFL needs to be added to the TLF -- ensure the same setting as in tsflab04
trtemfl <- TRUE

## ANL flag variable for low grade
anl_low_fl <- "ANL04FL"

## ANL flag variable for high grade
anl_high_fl <- "ANL05FL"



################################################################################
# Initial processing of data + check if table is valid for trial:
################################################################################
adlb_complete <- adlb_jnj 


################################################################################
# Process markedly abnormal values from spreadsheet:
################################################################################

### Markedly Abnormal spreadsheet

markedlyabnormal_file <- read_path(datapath, "markedlyabnormal.xlsx")


markedlyabnormal_sheets <- readxl::excel_sheets(markedlyabnormal_file)

lbmarkedlyabnormal_defs <- readxl::read_excel(
  markedlyabnormal_file,
  sheet = toupper(ad_domain)
) |>
  filter(PARAMCD != "Parameter Code")

MCRITs <- unique(
  lbmarkedlyabnormal_defs |>
    filter(!stringr::str_ends(VARNAME, "ML")) |>
    pull(VARNAME)
)


MCRITs_def <- unique(
  lbmarkedlyabnormal_defs |>
    filter(VARNAME %in% MCRITs) |>
    select(PARAMCD, VARNAME, CRIT, SEX)
) |>
  mutate(VARNAME = paste0(VARNAME, "ML")) |>
  rename(CRITNAME = CRIT) |>
  mutate(
    CRITDIR = case_when(
      VARNAME == "MCRIT1ML" ~ "DIR1",
      VARNAME == "MCRIT2ML" ~ "DIR2"
    )
  )


MCRITs_def2 <- lbmarkedlyabnormal_defs |>
  filter(VARNAME %in% paste0(MCRITs, "ML")) |>
  mutate(CRITn = as.character(4 - as.numeric(ORDER)))


MCRITs_def3 <- MCRITs_def2 |>
  left_join(MCRITs_def, relationship = "many-to-one") |>
  select(PARAMCD, CRITNAME, CRITDIR, SEX, VARNAME, CRIT, CRITn) |>
  arrange(PARAMCD, VARNAME, CRITDIR, SEX, CRITn) |>
  select(-SEX)


### convert dataframe into label_map that can be used with the a_freq_j afun function
xlabel_map <- MCRITs_def3 |>
  rename(var = VARNAME, label = CRIT) |>
  select(PARAMCD, CRITNAME, CRITDIR, var, label)


xlabel_map2 <- xlabel_map |>
  mutate(
    MCRIT12 = CRITNAME,
    MCRIT12ML = label
  )


################################################################################
# Process Data:
################################################################################

adsl <- adsl_jnj |>
  filter(.data[[popfl]] == "Y") |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c("Xanomeline Low Dose", "Xanomeline High Dose", "Placebo")
    )
  ) |>
  select(USUBJID, all_of(c(popfl, trtvar)))

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

obs_mcrit12 <- unique(c(
  unique(adlb_complete$MCRIT1),
  unique(adlb_complete$MCRIT2)
))

adlb00 <- adlb_complete |>
  filter(
    .data[[popfl]] == "Y" &
      PARCAT2 == "Test with FDA abnormality criteria defined" &
      toupper(PARCAT1) %in% toupper(unlist(parcat1_categories))
  ) |>
  select(
    USUBJID,
    PARCAT1,
    PARCAT2,
    PARCAT3,
    PARCAT3N,
    ONTRTFL,
    TRTEMFL,
    PARAM,
    PARAMCD,
    AVISITN,
    AVISIT,
    AVAL,
    MCRIT1,
    MCRIT1ML,
    MCRIT2,
    MCRIT2ML,
    LVOTFL,
    ANL01FL,
    ANL02FL,
    all_of(c(anl_low_fl, anl_high_fl))
    ### if per period/phase is needed, use below flag variables
    # ,ANL07FL,ANL08FL,ANL09FL,ANL10FL
  ) |>
  arrange(PARCAT1, PARCAT3N, PARCAT3, PARAM) |>
  mutate(PARAM = factor(as.character(PARAM), levels = unique(as.character(PARAM)))) |>
  inner_join(adsl)


################################################################################
##### Vertical approach for analyzing MCRIT1/MCRIT2:
# filtering is easier, as well as the analyze/layout setup
################################################################################

adlb_mcrit1 <- adlb00 |>
  filter(!is.na(MCRIT1)) |>
  mutate(
    MCRIT12 = MCRIT1,
    MCRIT12ML = MCRIT1ML,
    CRITDIR = "DIR1",
    ANLHLFL = .data[[anl_low_fl]]
  )

adlb_mcrit2 <- adlb00 |>
  filter(!is.na(MCRIT2)) |>
  mutate(
    MCRIT12 = MCRIT2,
    MCRIT12ML = MCRIT2ML,
    CRITDIR = "DIR2",
    ANLHLFL = .data[[anl_high_fl]]
  )

### note: by filter ANL04FL/ANL05FL (controlled by anl04fl/anl05fl parameters), this table is restricted to On-treatment values, per definition of ANL04FL/ANL05FL
### therefor, no need to add ONTRTFL in filter
### if derivation of ANL04FL/ANL05FL is not restricted to ONTRTFL records, adding ONTRTFL here will not give the correct answer either
### as mixing worst with other period is not giving the proper selection !!!

adlb_mcrit <- rbind(adlb_mcrit1, adlb_mcrit2) |>
  filter(ANLHLFL == "Y") |>
  mutate(
    PARAMCD := ordered(PARAMCD, levels = {
      # Sort by PARCAT3N then PARAMCD and PARCAT3 not displayed used only for sorting
      unique(PARAMCD[order(PARCAT3N, PARAM)])
    }),
    AVISIT = factor(
      .data[['AVISIT']],
      levels = unique(.data[['AVISIT']])[order(unique(.data[['AVISITN']]))]
    )
  ) |>
  inner_join(adsl)

#### DO NOT USE TRTEMFL = Y in filter, as this will remove subjects from both numerator and denominator
#### instead : set MCRIT12ML to a non-reportable value (ie Level 0) and keep in dataset
if (trtemfl) {
  origlevs <- levels(adlb_mcrit$MCRIT12ML)

  adlb_mcrit <- adlb_mcrit |>
    mutate(
      MCRIT12ML = case_when(
        !is.na(MCRIT12ML) & is.na(TRTEMFL) | TRTEMFL != "Y" ~ "Level 0",
        TRUE ~ MCRIT12ML
      )
    ) |>
    mutate(MCRIT12ML = factor(MCRIT12ML, levels = origlevs))
}


################################################################################
##### Finalize mapping dataframe based upon abnormal spreadsheet
################################################################################

xlabel_map3 <- xlabel_map2 |>
  right_join(unique(adlb_mcrit |> select(PARAMCD, PARCAT1, PARCAT3, PARCAT3N))) |>
  arrange(PARCAT1, PARCAT3N, PARAMCD, CRITDIR, MCRIT12, MCRIT12ML) |>
  mutate_if(is.factor, as.character) |>
  #### get rid of mapping defined in spreadsheet but not present in data
  filter(MCRIT12 %in% obs_mcrit12)

### this will ensure alphabetical sorting on abnormality
### within a test LOW needs to come prior to High
### for this reason, split a test like 'Calcium, low' and 'Calcium, High' in 2
xlabel_map3 <- xlabel_map3 |>
  mutate(MCRIT12x = stringr::str_split_i(MCRIT12, ",", 1)) |>
  arrange(PARCAT1, PARCAT3N, MCRIT12x, CRITDIR, MCRIT12ML)


# MCRIT12ML needs to be a factor, with all levels (also unobserved),
# as these levels are not available on the metadata files, only in markedly abnormal
# we need to update the factor levels
# these are present in the markedly abnormal file processing, ie we can use xlabel_map3

adlb_mcrit$MCRIT12ML <- factor(
  as.character(adlb_mcrit$MCRIT12ML),
  levels = unique(xlabel_map3$MCRIT12ML)
)

################################################################################
# Define layout and build table:
################################################################################

.extra_args_rr <- list(
  method = "wald",
  denom = "n_df",
  ref_path = ref_path,
  .stats = c("denom", "count_unique_fraction"),
  na_str = "-"
)


################################################################################
# Core function to produce table for a specific PARCAT1 selection
# Data sorted by PARCAT1 -> PARCAT3N -> PARAM
################################################################################

build_result_parcat1 <- function(
  df = adlb_mcrit,
  PARCAT1sel = NULL,
  tblid,
  .adsl = adsl,
  map = xlabel_map3,
  extra_args_rr = .extra_args_rr,
  .trtvar = trtvar,
  .ctrl_grp = ctrl_grp,
  .combined_colspan_trt = combined_colspan_trt
) {
  if (!is.null(PARCAT1sel)) {
    map <- map |> filter(toupper(PARCAT1) %in% toupper(PARCAT1sel))
    df <- df |> filter(toupper(PARCAT1) %in% toupper(PARCAT1sel))
  }

  if (nrow(df) == 0) {
    message(paste0("PARCAT1 [", PARCAT1sel, "] is not present on input dataset"))
    return(NULL)
  }

  lyt <- basic_table(show_colcounts = TRUE, colcount_format = "N=xx") |>
    split_cols_by(
      "colspan_trt",
      split_fun = trim_levels_to_map(map = colspan_trt_map)
    )

  if (.combined_colspan_trt == TRUE) {
    lyt <- lyt |> split_cols_by(.trtvar, split_fun = mysplit)
  } else {
    lyt <- lyt |> split_cols_by(.trtvar)
  }

  lyt <- lyt |>
    split_cols_by("rrisk_header", nested = FALSE) |>
    split_cols_by(
      .trtvar,
      labels_var = "rrisk_label",
      split_fun = remove_split_levels(.ctrl_grp)
    ) |>
    split_rows_by(
      "PARAMCD",
      split_label = "Laboratory Test",
      label_pos = "topleft",
      child_labels = "hidden",
      split_fun = trim_levels_to_map(map)
    ) |>
    # Low prior to High
    split_rows_by(
      "CRITDIR",
      label_pos = "hidden",
      child_labels = "hidden",
      split_fun = trim_levels_to_map(map)
    ) |>
    split_rows_by(
      "MCRIT12",
      split_label = "Threshold Level, n (%)",
      label_pos = "topleft",
      split_fun = trim_levels_to_map(map),
      section_div = " "
    ) |>
    # denominators are varying per test, therefor show denom (not yet in shell)
    analyze(
      c("MCRIT12ML"),
      a_freq_j,
      extra_args = append(extra_args_rr, NULL),
      show_labels = "hidden",
      indent_mod = 0L
    )

  result <- build_table(lyt, df, alt_counts_df = .adsl, round_type = "sas")

  ################################################################################
  # Post-Processing:
  ################################################################################
  remove_grade0 <- function(tr) {
    if (is(tr, "DataRow") & (tr@label == "Level 0")) {
      return(FALSE)
    } else if (is(tr, "DataRow") & (tr@label == no_data_to_report_str)) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }

  result <- result |> prune_table(prune_func = keep_rows(remove_grade0))
  result <- remove_col_count(result)

  ################################################################################
  # Add titles and footnotes:
  ################################################################################
  result <- set_titles(result, tab_titles)

  return(result)
}

################################################################################
# Define layout and build table:
################################################################################

result <- build_result_parcat1(
  PARCAT1sel = parcat1_categories[["hem"]],
  tblid = tblid
)

colwidth <- c(42, 19, 19, 21, 21, 33, 35)

tt_to_tlgrtf(
  colwidths = colwidth,
  result,
  file = fileid,
  orientation = "landscape"
)