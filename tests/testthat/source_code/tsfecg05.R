library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)
library(haven)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSFECG05"
fileid <- write_path(opath, tblid)
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

popfl <- "SAFFL"
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"

## if the option TRTEMFL needs to be added to the TLF
trtemfl <- TRUE

# flag to indclude timepoint rows
over_time <- TRUE


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


# Add Active Study Agent Combined column?
combined_colspan_trt <- TRUE

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
# initial read of data
################################################################################

adsl <- adsl_jnj |>
  filter(.data[[popfl]] == "Y") |>
  select(STUDYID, USUBJID, all_of(c(trtvar, popfl))) |>
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
  create_colspan_var(
    non_active_grp = ctrl_grp,
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = trtvar
  )


adeg_complete <- adeg_jnj

## check parameters that have CRIT1,CRIT2 defined in data
levels_data <- unique(adeg_complete |> select(PARAMCD, PARAM, PARAMN, CRIT1, CRIT2))

# restrict to these - ordered by PARAMN
selparamcd <- as.character(
  levels_data |>
    filter(!(is.na(CRIT1) | is.na(CRIT2))) |>
    arrange(PARAMN) |>
    pull(PARAMCD) |>
    unique()
)


################################################################################
# Mapping for CRIT1/2
################################################################################

xlabel_map <- levels_data |>
  tidyr::pivot_longer(
    cols = c("CRIT1", "CRIT2"),
    names_to = "var",
    values_to = "label"
  ) |>
  filter(!is.na(label)) |>
  mutate(
    label = as.character(label),
    var = paste0(var, "FL"),
    value = "Y"
  )

#### Note: this is not in line with the markedly abnormal file
## for both EGHRMN & PRAG: crit1 and crit2 are reversed
## crit1 and crit2 on data are consistent with the order specified in the shell : low/high

adeg <- adeg_complete |>
  filter(
    !is.na(USUBJID),
    .data[[popfl]] == "Y",
    !is.na(.data[[trtvar]]),
    PARAMCD %in% selparamcd
  ) |>
  mutate(
    CRIT1FL = ifelse(!is.na(CRIT1FL) & CRIT1FL == "Y", "Y", "N"),
    CRIT2FL = ifelse(!is.na(CRIT2FL) & CRIT2FL == "Y", "Y", "N"),
    PARAMCD := factor(PARAMCD, levels = selparamcd),
    PARAM := factor(PARAM),
    # Baseline records: label AVISIT as "Baseline" using ABLFL flag
    AVISIT := factor(as.character(AVISIT), levels = unique(.data[['AVISIT']])[order(unique(.data[['AVISITN']]))])
  ) |>
  select(
    STUDYID,
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
    TRTEMFL
  ) |>
  inner_join(adsl, by = c("STUDYID", "USUBJID"))

selvisit <- unique(as.character(adeg$AVISIT[adeg$AVISIT %in% selvisit]))

################################################################################
##### filter data
################################################################################

### alert on trtemfl, do not apply it as a filter, as this would lead to incorrect denominators

adeg_crit_any <- unique(
  adeg |>
    # synthetic data is missing the ANL04FL variable, however, this wouldn't be sufficient either, unless it flags both CRIT1 and CRIT2
    filter(ONTRTFL == "Y") |>
    mutate(AVISIT = factor("On-treatment")) |>
    select(-c(AVAL, ANL01FL, ANL02FL, ANL03FL))
)

### note: this does not necesarily leads to one record per parameter per subject
### if ANL04FL is available, we still can end up with more than one record
### a subject can have crit1fl=Y at one visit, and crit2fl=Y at another visit, and crit1fl=N&crit2fl=N at another
### as long as the analysis function deals with multiple records per subject correctly, this is not an issue

check_dup_sub <- adeg_crit_any |>
  group_by(USUBJID, PARAMCD, AVISIT) |>
  mutate(n_rec = n()) |>
  filter(n_rec > 1)

# Over time is also restricted to on treatment value
if (over_time) {
  adeg_crit_overtime <- adeg |>
    filter(ANL02FL == "Y" & AVISIT %in% selvisit & ONTRTFL == "Y") |>
    select(-c(AVAL, ANL01FL, ANL02FL, ANL03FL)) |>
    mutate(AVISIT = factor(as.character(AVISIT), levels = selvisit))

  adeg_crit_comb <- rbind(adeg_crit_any, adeg_crit_overtime) |>
    mutate(
      AVISIT = factor(
        as.character(AVISIT),
        levels = c("On-treatment", levels(adeg_crit_overtime$AVISIT))
      )
    ) |>
    inner_join(adsl)
} else {
  adeg_crit_comb <- adeg_crit_any |>
    mutate(AVISIT = factor(as.character(AVISIT), levels = "On-treatment")) |>
    inner_join(adsl)
}


#### DO NOT USE TRTEMFL = Y in filter, as this will remove subjects from both numerator and denominator
#### instead : set "CRIT2FL","CRIT1FL" to a non-reportable value (ie N) and keep in dataset
if (trtemfl) {
  adeg_crit_comb <- adeg_crit_comb |>
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

adeg_crit_comb <- adeg_crit_comb |>
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

lyt <- basic_table(show_colcounts = TRUE, colcount_format = "N=xx") |>
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

lyt <- lyt |>
  split_rows_by(
    "PARAMCD",
    labels_var = "PARAM",
    split_label = "Parameter",
    label_pos = "topleft",
    split_fun = drop_split_levels
  ) |>
  split_rows_by(
    "AVISIT",
    split_label = "Study Visit",
    label_pos = "topleft",
    section_div = " ",
    split_fun = drop_split_levels
  ) |>
  analyze(
    c("CRIT1"),
    a_freq_j,
    extra_args = extra_args_rr1,
    show_labels = "hidden",
    indent_mod = 0L
  ) |>
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
  ) |>
  append_topleft("    Criteria, n (%)")

result <- build_table(lyt, adeg_crit_comb, alt_counts_df = adsl, round_type = "sas")


################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table
################################################################################


colwidth <- c(56, 21, 23, 23, 21)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid)
