library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSFVIT02"
fileid <- write_path(opath, tblid)
titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

popfl <- "SAFFL"
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"

selparamcd <- c("SYSBP", "DIABP")

###criteria flags to consider for output
crit_flags <- paste0('CRIT', 1:6, 'FL')

### as in dataset, order is important for later processing
### not automated, hard coded approach for ease of reading
### ideally the datasets already contain the appropriate case, to ensure units are in proper case
sel_param <- c("Systolic Blood Pressure (mmHg)", "Diastolic Blood Pressure (mmHg)")

### Parameter headers label
sel_param_case <- c("Maximum systolic blood pressure (mmHg)", "Maximum diastolic blood pressure (mmHg)")

relrisk <- TRUE
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
  select(
    USUBJID,
    all_of(c(popfl, trtvar)),
    SEX,
    AGEGR1,
    RACE,
    ETHNIC
  )

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

advs00 <- advs_jnj |>
  filter(PARAMCD %in% selparamcd) |>
  select(
    STUDYID,
    USUBJID,
    PARAMCD,
    PARAM,
    AVALCAT1,
    AVALCA1N,
    AVISIT,
    ANL03FL,
    ANL06FL,
    APOBLFL,
    starts_with("CRIT")
  ) |>
  inner_join(adsl) |>
  # Factor the data columns to ensure they sort correctly in the output
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c("Xanomeline Low Dose", "Xanomeline High Dose", "Placebo")
    )
  )

### note: by filter ANL03FL, this table is restricted to maximum On-treatment values, per definition of ANL03FL
filtered_advs <- advs00 |>
  filter(ANL03FL == 'Y')

# Factor the data columns to ensure they sort correctly in the output
filtered_advs$PARAM <- factor(
  filtered_advs$PARAM,
  levels = c(as.character(sel_param)),
  labels = c(as.character(sel_param_case))
)

### Systolic parameter
filtered_advs1 <- filtered_advs |>
  filter(PARAM == sel_param_case[1])

### Systolic parameter
filtered_advs2 <- filtered_advs |>
  filter(PARAM == sel_param_case[2])

### note: by filter ANL06FL, this table is restricted to On-treatment values, per definition of ANL06FL
filtered_advs3 <- advs00 |>
  filter(ANL06FL == 'Y') |>
  mutate(
    CRIT1FL_SYS = case_when(
      PARAMCD == 'SYSBP' ~ CRIT1FL
    ),
    CRIT1FL_DIA = case_when(
      PARAMCD == 'DIABP' ~ CRIT1FL
    ),
    PARAM = "Hypotension (mmHg)",
    CRIT1 = case_when(
      PARAMCD == "SYSBP" ~ "Systolic blood pressure <90",
      PARAMCD == "DIABP" ~ "Diastolic blood pressure <60",
      TRUE ~ as.character(CRIT1)
    )
  )

#### create a label data frame to use in labels_map argument on analysis of each criteria flag
crit_cat <- sub('FL$', "", crit_flags)

xlabel_map_sys <- unique(filtered_advs1 |> select(PARAM, all_of(crit_cat))) |>
  tidyr::pivot_longer(
    cols = all_of(crit_cat),
    names_to = "var",
    values_to = "label"
  ) |>
  filter(!is.na(label)) |>
  mutate(
    label = as.character(label),
    var = paste0(var, "FL"),
    value = "Y"
  )

xlabel_map_dia <- unique(filtered_advs2 |> select(PARAM, all_of(crit_cat))) |>
  tidyr::pivot_longer(
    cols = all_of(crit_cat),
    names_to = "var",
    values_to = "label"
  ) |>
  filter(!is.na(label)) |>
  mutate(
    label = as.character(label),
    var = paste0(var, "FL"),
    value = "Y"
  )

### create a label data frame for Hypotension parameter for separate section
xlabel_map_hyps <- unique(filtered_advs3 |> filter(PARAMCD == selparamcd[1]) |> select(PARAM, CRIT1)) |>
  tidyr::pivot_longer(
    cols = c("CRIT1"),
    names_to = "var",
    values_to = "label"
  ) |>
  filter(!is.na(label)) |>
  mutate(
    label = as.character(label),
    var = paste0(var, "FL_SYS"),
    value = "Y"
  )


xlabel_map_hypd <- unique(filtered_advs3 |> filter(PARAMCD == selparamcd[2]) |> select(PARAM, CRIT1)) |>
  tidyr::pivot_longer(
    cols = c("CRIT1"),
    names_to = "var",
    values_to = "label"
  ) |>
  filter(!is.na(label)) |>
  mutate(
    label = as.character(label),
    var = paste0(var, "FL_DIA"),
    value = "Y"
  )

################################################################################
# Define layout and build table:
################################################################################

extra_args1 <- list(denom = "n_df", riskdiff = FALSE, .stats = c("n_df"))
extra_args_rr <- list(
  denom = "n_df",
  riskdiff = TRUE,
  method = "wald",
  ref_path = ref_path,
  .stats = c("count_unique_fraction")
)

lyt0 <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) |>
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  )

if (combined_colspan_trt == TRUE) {
  lyt0 <- lyt0 |>
    split_cols_by(trtvar, split_fun = mysplit)
} else {
  lyt0 <- lyt0 |>
    split_cols_by(trtvar)
}

if (relrisk) {
  lyt0 <- lyt0 |>
    split_cols_by("rrisk_header", nested = FALSE) |>
    split_cols_by(
      trtvar,
      labels_var = "rrisk_label",
      split_fun = remove_split_levels(ctrl_grp)
    )
}

lyt1 <- lyt0 |>
  split_rows_by(
    "PARAM",
    label_pos = "topleft",
    parent_name = "PARAM_SD_N",
    labels_var = "PARAM",
    child_labels = "visible",
    split_label = "Parameter, n (%)",
    section_div = " ",
    ## ensure only the appropriate levels inside PARAM-AVALCAT1 will be included
    split_fun = trim_levels_in_group('PARAM')
  ) |>
  #### this assumes subjects always have both systolic and diastolic parameters
  analyze(
    "CRIT1FL",
    a_freq_j,
    show_labels = "hidden",
    table_names = "CRIT1_SYS_N",
    extra_args = extra_args1
  ) |>
  #CRIT1FL
  analyze(
    crit_flags,
    a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(
        val = "Y",
        label_map = xlabel_map_sys
      )
    ),
    indent_mod = 1L,
    show_labels = "hidden"
  )

lyt2 <- lyt0 |>
  split_rows_by(
    "PARAM",
    label_pos = "topleft",
    parent_name = "PARAM_SD_N",
    labels_var = "PARAM",
    child_labels = "visible",
    split_label = "Parameter, n (%)",
    section_div = " ",
    ## ensure only the appropriate levels inside PARAM-AVALCAT1 will be included
    split_fun = trim_levels_in_group('PARAM')
  ) |>
  #### this assumes subjects always have both systolic and diastolic parameters
  analyze(
    "CRIT1FL",
    a_freq_j,
    show_labels = "hidden",
    table_names = "CRIT1_DIA_N",
    extra_args = extra_args1
  ) |>
  #CRIT1FL
  analyze(
    crit_flags,
    a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(
        val = "Y",
        label_map = xlabel_map_dia
      )
    ),
    indent_mod = 1L,
    show_labels = "hidden"
  )

lyt3 <- lyt0 |>
  split_rows_by(
    "PARAM",
    label_pos = "topleft",
    parent_name = "PARAM_HYP_N",
    labels_var = "PARAM",
    child_labels = "visible",
    split_label = "Parameter, n (%)",
    section_div = " ",
    ## ensure only the appropriate levels inside PARAM-AVALCAT1 will be included
    split_fun = drop_split_levels
  ) |>
  #### this assumes subjects always have both systolic and diastolic parameters
  analyze(
    "CRIT1FL",
    a_freq_j,
    show_labels = "hidden",
    table_names = "CRIT1_HYP_N",
    extra_args = extra_args1
  ) |>
  #CRIT1FL_SYS
  analyze(
    "CRIT1FL_SYS",
    a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(
        val = "Y",
        label_map = xlabel_map_hyps
      )
    ),
    indent_mod = 1L,
    show_labels = "hidden"
  ) |>
  #CRIT1FL_DIA
  analyze(
    "CRIT1FL_DIA",
    a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(
        val = "Y",
        label_map = xlabel_map_hypd
      )
    ),
    indent_mod = 1L,
    show_labels = "hidden"
  )


result_s1 <- build_table(lyt1, df = filtered_advs1, alt_counts_df = adsl, round_type = "sas")
result_s2 <- build_table(lyt2, df = filtered_advs2, alt_counts_df = adsl, round_type = "sas")
result_s3 <- build_table(lyt3, df = filtered_advs3, alt_counts_df = adsl, round_type = "sas")

## stacking layout of two sections with different subset
result <- rtables::rbind(result_s1, result_s2, result_s3)
################################################################################
# Post-Processing:
# - remove unwanted colcounts
################################################################################

result <- remove_col_count(result)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, titles)

################################################################################
# Convert to tbl file and output table
################################################################################


colwidth <- c(64, 21, 21, 21, 33, 33)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "landscape")
