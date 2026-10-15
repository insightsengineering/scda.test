library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSFVIT03"
fileid <- write_path(opath, tblid)
titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

popfl <- "SAFFL"
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"

selparamcd <- c("SYSBP", "DIABP", "PULSE")

### as in dataset, order is important for later processing
### not automated, hard coded approach for ease of reading
### ideally the datasets already contain the appropriate case, to ensure units are in proper case
sel_param <- c(
  "Systolic Blood Pressure (mmHg)",
  "Diastolic Blood Pressure (mmHg)",
  "Pulse Rate (beats/min)"
)
sel_param_case <- c(
  "Systolic blood pressure (mmHg)",
  "Diastolic blood pressure (mmHg)",
  "Pulse rate (beats/min)"
)

## different variants of table
over_time <- TRUE # over time analysis, ALSO restricted to on-treatment records
worst <- TRUE # analysis of min/max value, restricted to on-treatment records
## you can set both worst and over_time to TRUE, the "worst" timepoint will be shown first
treatment_emergent <- TRUE # only consider te values

relrisk <- FALSE
# by default JJCS there is no need to add relative risk columns

combined_colspan_trt <- FALSE

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

################################################################################
# Process Data:
################################################################################

adsl <- adsl_jnj |>
  filter(.data[[popfl]] == "Y") |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c(
        "Xanomeline Low Dose",
        "Xanomeline High Dose",
        "Placebo"
      )
    )
  ) |>
  select(USUBJID, all_of(c(popfl, trtvar)), SEX, AGEGR1, RACE, ETHNIC)

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

### for analysis of treatment emergent flag, do not use TRTEMFL as filter, as this will not give the proper denominator

if (over_time) {
  ## Version 1 : over time
  filtered_advs_1 <- advs_jnj |>
    filter(PARAMCD %in% selparamcd) |>
    filter(ANL02FL == "Y") |>
    ### note regarding ONTRTFL --- Per agreement on June 17 : On treatment over time : Filter on ONTRTFL as well
    ### updated APOBLFL instead of ONTRTFL as per annotation update
    ### Apr 2026: updated code to consider ONTRTFL instead of APOBLFL flag as per annotation
    filter(ONTRTFL == "Y") |>
    select(
      STUDYID,
      USUBJID,
      PARAMCD,
      PARAM,
      PARAMN,
      AVALCAT1,
      AVALCA1N,
      AVISIT,
      AVISITN,
      ONTRTFL,
      APOBLFL,
      CRIT7,
      CRIT7FL,
      CRIT8,
      CRIT8FL,
      TRTEMFL,
      ANL02FL,
      AVAL
    ) |>
    inner_join(adsl)
}

if (worst) {
  ## Version 2 : "worst analysis" -- ANL03FL (max) ANL06FL (min)

  ### note: by filter ANL06FL/ANL03FL, this table is restricted to On-treatment values, per definition of ANL06FL/ANL03FL
  ### therefore, no need to add ONTRTFL in filter
  ### if derivation of ANL06FL is not restricted to ONTRTFL records, adding ONTRTFL here will not give the correct answer either
  ### as mixing worst with other period is not giving the proper selection !!!
  ### June 2026: Added additional filter on ONTRTFL as per annotation

  filtered_advs_2 <- advs_jnj |>
    filter(PARAMCD %in% selparamcd) |>
    # 2 records per subject selected : min (ANL06) / max (ANL03)
    filter(ANL03FL == "Y" | ANL06FL == "Y" & ONTRTFL == 'Y') |>
    select(
      STUDYID,
      USUBJID,
      PARAMCD,
      PARAM,
      PARAMN,
      AVALCAT1,
      AVALCA1N,
      AVISIT,
      AVISITN,
      APOBLFL,
      CRIT7,
      CRIT7FL,
      CRIT8,
      CRIT8FL,
      TRTEMFL,
      ANL03FL,
      ANL06FL,
      AVAL
    ) |>
    inner_join(adsl)

  ### need to check for your study if the below applies as these can be study specific
  ## for CRIT7 : use minimum (ANL06FL) - disregard the record if it was not coming from MIN
  ## for CRIT8 : use maximum (ANL03FL) - disregard the record if it was not coming from MAX
  ## 1 record per subjects with non-missing CRIT7FL
  ## 1 record per subjects with non-missing CRIT8FL
  ## these will be over 2 different records
  filtered_advs_2 <- filtered_advs_2 |>
    mutate(
      CRIT7FL = case_when(
        ANL06FL == "N" ~ NA,
        TRUE ~ CRIT7FL
      ),
      CRIT8FL = case_when(
        ANL03FL == "N" ~ NA,
        TRUE ~ CRIT8FL
      )
    ) |>
    # On-treatment records assigned visit number 00 to display first in ordering
    mutate(AVISIT = "On-treatment", AVISITN = 00)
}


if (over_time & worst) {
  filtered_advs <- bind_rows(
    filtered_advs_1,
    filtered_advs_2
  )
} else if (over_time) {
  filtered_advs <- filtered_advs_1
} else if (worst) {
  filtered_advs <- filtered_advs_2
} else {
  stop("At least worst or over_time should be set to TRUE")
}

filtered_advs$PARAM <- factor(
  as.character(filtered_advs$PARAM),
  levels = sel_param,
  labels = sel_param_case
)
### reorder AVISIT factor levels by it's AVISTN order
filtered_advs$AVISIT <- factor(
  as.character(filtered_advs$AVISIT),
  levels = unique(filtered_advs[['AVISIT']])[order(unique(filtered_advs[['AVISITN']]))]
)

### for analysis of treatment emergent abnormalities only :
### per discussion with DAS team: denominator is still the subjects with evaluation of CRIT7FL/CRIT8FL,
### there is no need to get baseline status to exclude subjects with an abnormal baseline status
### the only thing that should be done is : non-treatment emergent abnormalities should not be reported in the numerator, only in denom
### this can be done using the following code

if (treatment_emergent) {
  filtered_advs <- filtered_advs |>
    mutate(
      CRIT7FL = case_when(
        CRIT7FL == "Y" & (is.na(TRTEMFL) | TRTEMFL != "Y") ~ "N",
        TRUE ~ CRIT7FL
      ),
      CRIT8FL = case_when(
        CRIT8FL == "Y" & (is.na(TRTEMFL) | TRTEMFL != "Y") ~ "N",
        TRUE ~ CRIT8FL
      )
    )
}

### converting CRIT7FL/CRIT8FL to factor for use in label maps on layout
filtered_advs$CRIT7FL <- factor(
  as.character(filtered_advs$CRIT7FL),
  levels = c("Y", "N")
)

filtered_advs$CRIT8FL <- factor(
  as.character(filtered_advs$CRIT8FL),
  levels = c("Y", "N")
)

filtered_advs$PARAM <- factor(
  filtered_advs$PARAM,
  levels = unique(filtered_advs[['PARAM']])[order(unique(filtered_advs[['PARAMN']]))]
)

filtered_advs$AVISIT <- factor(
  filtered_advs$AVISIT,
  levels = unique(filtered_advs[['AVISIT']])[order(unique(filtered_advs[['AVISITN']]))]
)

### Mapping for CRIT7&CRIT8
### alternative approach to retrieve from metadata iso dataset -
xlabel_map <- unique(filtered_advs |> select(PARAM, CRIT7, CRIT8)) |>
  tidyr::pivot_longer(
    cols = c("CRIT7", "CRIT8"),
    names_to = "var",
    values_to = "label"
  ) |>
  filter(!is.na(label)) |>
  mutate(
    label = as.character(label),
    var = paste0(var, "FL"),
    value = "Y"
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
) |>
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  )

if (combined_colspan_trt == TRUE) {
  lyt <- lyt |>
    split_cols_by(trtvar, split_fun = mysplit)
} else {
  lyt <- lyt |>
    split_cols_by(trtvar)
}

if (relrisk) {
  lyt <- lyt |>
    split_cols_by("rrisk_header", nested = FALSE) |>
    split_cols_by(
      trtvar,
      labels_var = "rrisk_label",
      split_fun = remove_split_levels(ctrl_grp)
    )
}

lyt <- lyt |>
  split_rows_by(
    "AVISIT",
    label_pos = "topleft",
    child_labels = "visible",
    split_label = "Study Visit",
    split_fun = drop_split_levels,
    section_div = " "
  ) |>
  split_rows_by(
    "PARAM",
    label_pos = "topleft",
    child_labels = "visible",
    split_label = "Parameter",
    split_fun = drop_split_levels,
    section_div = " "
  ) |>
  summarize_row_groups(
    "AVISIT",
    cfun = a_freq_j,
    extra_args = list(
      .stats = "n_df",
      label = "N",
      riskdiff = FALSE
    )
  ) |>
  analyze(
    c("CRIT7FL", "CRIT8FL"),
    a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(val = c("Y"), label_map = xlabel_map)
    ),
    show_labels = "hidden",
    indent_mod = 1L
  ) |>
  append_topleft("   Criteria, n (%)")

result <- build_table(lyt, filtered_advs, alt_counts_df = adsl, round_type = "sas")

################################################################################
# Post-Processing:
# - remove unwanted colcounts
################################################################################
if (over_time) {
  visit_list <- unique(as.character(filtered_advs$AVISIT))

  for (i in seq_along(visit_list)) {
    section_div_at_path(result, path = c("AVISIT", visit_list[[i]]), labelrow = TRUE) <- " "
  }
} else {
  section_div_at_path(result, path = c("PARAM", "*", "CRIT8FL"), labelrow = TRUE) <- " "
}

result <- remove_col_count(result)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, titles)

################################################################################
# Convert to tbl file and output table
################################################################################


colwidth <- c(64, 7, 7, 7)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid)
