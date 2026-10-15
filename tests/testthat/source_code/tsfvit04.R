library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSFVIT04"
fileid <- write_path(opath, tblid)
titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

popfl <- "SAFFL"
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"

# flag to indclude timepoint rows
over_time <- TRUE

# only consider TE values
treatment_emergent <- FALSE

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

# specify in the order you want to print on table
selparamcd <- c("ORTHYP", "ORTHYPS", "ORTHYPD")

selparamcdN <- tibble(PARAMCD = selparamcd, PARAMCDN = seq_along(selparamcd))

### Per email June 12: DAS/SDS confirmed to NOT restrict to on-treatment values

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

### N is the number of subjects with postbaseline orthostatic measurements and without orthostatic hypotension at baseline
## do not use TRTEMFL as filter, as this will only select AVALC = Y records per definition of TRTEMFL
## instead : start from post-baseline records and retain one record per subject
## for those subjects with both Y and N records, keep the Y record
## for those subjects with only Y records or only N records, keep one Y, N record respectively
filtered_advs_1 <- advs_jnj |>
  filter(PARAMCD %in% selparamcd & ONTRTFL == "Y") |>
  select(
    STUDYID,
    USUBJID,
    PARAMCD,
    PARAM,
    AVISIT,
    AVISITN,
    APOBLFL,
    TRTEMFL,
    CRIT1,
    CRIT1FL,
    ONTRTFL,
    AVALC
  ) |>
  inner_join(adsl) |>
  ### ensure to keep only 1 result per subject, keep N only in case no Y was observed
  arrange(USUBJID, PARAMCD, AVALC, CRIT1, CRIT1FL) |>
  group_by(USUBJID, PARAMCD) |>
  mutate(ncrit1 = n_distinct(CRIT1FL)) |>
  filter(!(ncrit1 > 1 & CRIT1FL == "N")) |>
  ## only keep one record
  slice_head(n = 1) |>
  ungroup() |>
  # On-treatment records assigned visit number 00 to display first in ordering
  mutate(AVISIT = "On-treatment", AVISITN = 00)

if (over_time) {
  filtered_advs_2 <- advs_jnj |>
    filter(PARAMCD %in% selparamcd & ONTRTFL == "Y") |>
    select(
      STUDYID,
      USUBJID,
      PARAMCD,
      PARAM,
      AVISIT,
      AVISITN,
      APOBLFL,
      TRTEMFL,
      CRIT1,
      CRIT1FL,
      ONTRTFL,
      AVALC
    ) |>
    inner_join(adsl) |>
    ### ensure to keep only 1 result per subject, keep N only in case no Y was observed
    arrange(USUBJID, PARAMCD, AVISIT, AVALC, CRIT1, CRIT1FL) |>
    group_by(USUBJID, PARAMCD, AVISIT) |>
    mutate(ncrit1 = n_distinct(CRIT1FL)) |>
    filter(!(ncrit1 > 1 & CRIT1FL == "N")) |>
    ## only keep one record
    slice_head(n = 1) |>
    ungroup()
}

if (over_time) {
  filtered_advs <- bind_rows(
    filtered_advs_1,
    filtered_advs_2
  )
} else {
  filtered_advs <- filtered_advs_1
}

### reorder AVISIT factor levels by it's AVISTN order
filtered_advs$AVISIT <- factor(
  as.character(filtered_advs$AVISIT),
  levels = unique(filtered_advs[["AVISIT"]])[order(unique(filtered_advs[["AVISITN"]]))]
)

#### remove subjects abnormal for "ORTHYP","ORTHYPS" & "ORTHYPD" at baseline
bl_abn_orthyp <- advs_jnj |>
  filter(PARAMCD %in% c("ORTHYP", "ORTHYPS", "ORTHYPD") & ABLFL == "Y" & AVALC == "Y")

### actually remove the subjects with AVALC = Y for ORTHYP
### N is the number of subjects with postbaseline orthostatic measurements and without orthostatic hypotension at baseline
filtered_advs <- filtered_advs |>
  filter(!(USUBJID %in% unique(bl_abn_orthyp$USUBJID))) |>
  ### For ORTHYP parameter AVALC='Y' should be considered and CRIT1FL considered for ORTHYPS & ORTHYPD as per annotation
  mutate(
    CRIT1FL = case_when(
      AVALC == "Y" & PARAMCD == "ORTHYP" ~ "Y",
      TRUE ~ CRIT1FL
    )
  )

if (treatment_emergent) {
  filtered_advs <- filtered_advs |>
    mutate(
      CRIT1FL = case_when(
        CRIT1FL == "Y" & (is.na(TRTEMFL) | TRTEMFL != "Y") ~ "N",
        TRUE ~ CRIT1FL
      )
    )
}

### get sorting as per order in selparamcdN
selparamcdN <- selparamcdN |>
  left_join(unique(filtered_advs |> select(PARAMCD, PARAM))) |>
  arrange(PARAMCDN)

param_levels <- unique(as.character(selparamcdN$PARAM))

filtered_advs$PARAM <- factor(
  as.character(filtered_advs$PARAM),
  levels = param_levels
)
### converting CRIT1FL to factor for use in label maps on layout
filtered_advs$CRIT1FL <- factor(
  as.character(filtered_advs$CRIT1FL)
)

### Mapping for AVALC
### alternative approach to retrieve from metadata iso dataset
xlabel_map <- unique(filtered_advs |> select(PARAM, PARAMCD, CRIT1)) |>
  tidyr::pivot_longer(
    cols = c("CRIT1"),
    names_to = "var",
    values_to = "label"
  ) |>
  filter(!is.na(label)) |>
  mutate(
    label = as.character(label),
    var = paste0(var, "FL"),
    value = "Y"
  ) |>
  mutate(
    label = case_when(
      label == "SBP (STD-SUP)<-20 or DBP (STD-SUP)<-10" ~ "Total number of subjects with orthostatic hypotension",
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
    split_fun = drop_split_levels,
    child_labels = "visible",
    split_label = " ",
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
  split_rows_by(
    "PARAM",
    label_pos = "topleft",
    split_fun = drop_split_levels,
    child_labels = "hidden",
    split_label = "Orthostatic Hypotension, n (%)"
  ) |>
  analyze(
    "CRIT1FL",
    a_freq_j,
    table_names = "CRIT1_V1",
    extra_args = append(
      extra_args_rr,
      list(
        val = c("Y"),
        label_map = xlabel_map
      )
    ),
    indent_mod = 1L,
    show_labels = "hidden"
  )

result <- build_table(lyt, filtered_advs, alt_counts_df = adsl, round_type = "sas")

################################################################################
# Post-Processing:
# - update indent for "SYSBPO" and "DIABPO"
# - remove unwanted colcounts
################################################################################

## update indent for "ORTHYPS" and "ORTHYPD"
adj_indent_mod <- function(result, path, indentupd) {
  indent_mod(tt_at_path(result, path)) <- indent_mod(tt_at_path(result, path)) +
    indentupd
  return(result)
}

avisit_ind <- unique(as.character(filtered_advs$AVISIT))

for (i in seq_along(avisit_ind)) {
  path_ <- c("AVISIT", avisit_ind[[i]], "PARAM", "Orthostatic Hypotension")

  result <- adj_indent_mod(
    result,
    path = path_,
    indentupd = -1L
  )
}

result <- remove_col_count(result)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, titles)

################################################################################
# Convert to tbl file and output table
################################################################################


colwidth <- c(64, 21, 21, 21)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid)
