#### BEFORE YOU START USING THIS PROGRAM ENSURE THE FOLLOWING: For your trial you should EITHER use lab toxicity grading (lbtoxgrade file) or Abnormality criteria (markedly abnormal file)
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

tblid <- "TSFLAB02b"
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

selvisit <- c("Screening", "Cycle 02", "Cycle 03", "Cycle 04")


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
  filter(!is.na(PARCAT2)) |>
  select(
    USUBJID,
    PARCAT1,
    PARCAT2,
    PARCAT3,
    PARCAT3N,
    ONTRTFL,
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
    ABLFL,
    ANL01FL,
    ANL02FL,
    ANL04FL,
    ANL05FL
    ### if per period/phase is needed, use below flag variables
    # ,ANL07FL,ANL08FL,ANL09FL,ANL10FL
  ) |>
  inner_join(adsl)

################################################################################
##### vertical approach for analyzing MCRIT1/MCRIT2:
# filtering is easier, as well as the analyze/layout setup
################################################################################

adlb_mcrit1 <- adlb00 |>
  filter(!is.na(MCRIT1)) |>
  mutate(
    MCRIT12 = MCRIT1,
    MCRIT12ML = MCRIT1ML,
    CRITDIR = "DIR1"
  )

adlb_mcrit2 <- adlb00 |>
  filter(!is.na(MCRIT2)) |>
  mutate(
    MCRIT12 = MCRIT2,
    MCRIT12ML = MCRIT2ML,
    CRITDIR = "DIR2"
  )

adlb_mcrit <- rbind(adlb_mcrit1, adlb_mcrit2) |>
  mutate(
    CRITDIR = factor(CRITDIR),
    PARAMCD := ordered(PARAMCD, levels = {
      # Sort by PARCAT3N then PARAMCD and PARCAT3 not displayed used only for sorting
      unique(PARAMCD[order(PARCAT3N, PARAM)])
    })
  ) |>
  filter(AVISIT %in% selvisit) |>
  ### unique record per timepoint:
  filter(ANL02FL == "Y")

################################################################################
##### finalize mapping dataframe based upon abnormal spreadsheet
################################################################################

xlabel_map3 <- xlabel_map2 |>
  right_join(unique(adlb_mcrit |> select(PARAMCD, PARCAT1, PARCAT3N, PARCAT3))) |>
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

adlb_mcrit$AVISIT <- factor(
  adlb_mcrit$AVISIT,
  levels = unique(adlb_mcrit$AVISIT)[order(unique(adlb_mcrit$AVISITN))]
)

################################################################################
# Define layout and build table:
################################################################################

.extra_args_rr <- list(
  method = "wald",
  denom = "n_df",
  ref_path = ref_path,
  .stats = c("count_unique_fraction"),
  na_str = "-"
)

################################################################################
# Core function to produce shell for specific parcat3 selection
################################################################################

build_result_parcat1 <- function(
  df = adlb_mcrit,
  PARCAT1sel = NULL,
  tblid,
  .adsl = adsl,
  map = xlabel_map3,
  save2rtf = TRUE,
  extra_args_rr = .extra_args_rr,
  .trtvar = trtvar,
  .ref_path = ref_path,
  .ctrl_grp = ctrl_grp,
  .combined_colspan_trt = combined_colspan_trt
) {
  lyt_filter <- function(PARCAT1sel = NULL, map) {
    if (!is.null(PARCAT1sel)) {
      map <- map |>
        filter(toupper(PARCAT1) %in% toupper(PARCAT1sel))
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
      )

    lyt <- lyt |>
      split_rows_by(
        "PARAMCD",
        label_pos = "hidden",
        child_labels = "hidden",
        indent_mod = 1L,
        split_fun = trim_levels_to_map(map)
      ) |>
      ## Low prior to High
      split_rows_by(
        "CRITDIR",
        label_pos = "hidden",
        child_labels = "hidden",
        split_fun = trim_levels_to_map(map)
      ) |>
      split_rows_by(
        "MCRIT12",
        split_label = "Laboratory Test",
        label_pos = "topleft",
        split_fun = trim_levels_to_map(map),
        child_labels = "hidden",
        section_div = " "
      ) |>
      summarize_row_groups(
        var = "MCRIT12",
        cfun = a_freq_j,
        extra_args = list(
          .stats = "n_df",
          riskdiff = FALSE
        ),
        indent_mod = -1L
      )

    ### add in by avisit processing
    lyt <- lyt |>
      split_rows_by(
        "AVISIT",
        label_pos = "topleft",
        section_div = " ",
        child_labels = "visible",
        split_fun = drop_split_levels,
        split_label = "Study Visit"
      ) |>
      ### to mimic layout if analyze would be used instead
      ### child_labels has been set to visible in previous step
      summarize_row_groups(
        "AVISIT",
        cfun = a_freq_j,
        extra_args = list(
          .stats = "n_df",
          label = "N",
          riskdiff = FALSE
        )
      ) |>
      append_topleft("    Threshold Level, n (%)") |>
      # denominators are varying per test, no need to show as N is shown in line above
      analyze(
        c("MCRIT12ML"),
        a_freq_j,
        extra_args = extra_args_rr,
        show_labels = "hidden"
      )

    return(lyt)
  }

  lyt <- lyt_filter(PARCAT1sel = PARCAT1sel, map = map)

  if (!is.null(PARCAT1sel)) {
    df <- df |>
      filter(toupper(PARCAT1) %in% toupper(PARCAT1sel))
  }

  if (nrow(df) > 0) {
    result <- build_table(lyt, df, alt_counts_df = .adsl, round_type = "sas")
  } else {
    result <- NULL
    message(paste0(
      "PARCAT1 [",
      PARCAT1sel,
      "] is not present on input dataset"
    ))
    return(result)
  }

  ################################################################################
  # Remove Level 0 line
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

  ################################################################################
  # Remove unwanted column counts
  ################################################################################

  result <- remove_col_count(result)

  ################################################################################
  # Set title
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

colwidth <- c(44, 21, 21, 21, 21, 35, 35)

fileid <- write_path(opath, tblid)

tt_to_tlgrtf(
  colwidths = colwidth,
  result,
  file = fileid,
  orientation = "landscape"
)