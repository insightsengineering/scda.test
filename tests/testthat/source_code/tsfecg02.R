library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)
library(haven)

################################################################################
# Define script level parameters:
################################################################################

################################################################################
# - Define output ID and file location
# - Define treatment variable used (default=TRT01A)
# - Define population flag used (default=SAFFL)
# - Define control group label
# - Choose whether or not you want to present a combined active treatment column (default=TRUE)
################################################################################

tblid <- "TSFECG02"
fileid <- write_path(opath, tblid)
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

popfl <- "SAFFL"
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"

# Add Active Study Agent Combined column?
combined_colspan_trt <- TRUE

selparamcd <- c("QTCFAG", "QTCBAG", "QTCS")


selvisit <- c(
  "Baseline",
  "Month 1",
  "Month 3",
  "Month 6",
  "Month 9",
  "Month 12"
)

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
    STUDYID,
    USUBJID,
    all_of(c(popfl, trtvar))
  ) |>
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

filtered_adeg <- adeg_jnj

### available QT Interval parameters in study
selparamcd <- intersect(selparamcd, unique(filtered_adeg$PARAMCD))

filtered_adeg <- filtered_adeg |>
  filter(
    !is.na(USUBJID),
    .data[[popfl]] == "Y",
    !is.na(.data[[trtvar]]),
    PARAMCD %in% selparamcd,
    AVISIT %in% selvisit,
    !is.na(AVALCAT1),
    ANL02FL == "Y" & (ABLFL == "Y" | APOBLFL == "Y")
  ) |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c(
        "Xanomeline Low Dose",
        "Xanomeline High Dose",
        "Placebo"
      )
    )
  )


# restrict to these - ordered by PARAMN
selparamcd <- as.character(
  filtered_adeg |> arrange(PARAMN) |> pull(PARAMCD) |> unique()
)

filtered_adeg <- filtered_adeg |>
  mutate(
    ABLFL := factor(ifelse(!is.na(ABLFL) & ABLFL == "Y", "Y", "N")),
    APOBLFL := factor(ifelse(!is.na(APOBLFL) & APOBLFL == "Y", "Y", "N")),
    PARAMCD := factor(PARAMCD, levels = selparamcd),
    PARAM := factor(PARAM),
    AVISIT = factor(
      ifelse(ABLFL == "Y" & !is.na(ABLFL), "Baseline", as.character(AVISIT)),
      levels = unique(.data[['AVISIT']])[order(unique(.data[['AVISITN']]))]
    )
  ) |>
  select(
    STUDYID,
    USUBJID,
    PARAMCD,
    PARAM,
    PARAMN,
    AVALCAT1,
    AVALCA1N,
    AVISIT,
    ANL02FL,
    APOBLFL,
    ABLFL,
    ONTRTFL,
    all_of(c(popfl, trtvar)),
  ) |>
  inner_join(adsl, by = c("STUDYID", "USUBJID", popfl, trtvar))

selvisit <- unique(as.character(filtered_adeg$AVISIT[filtered_adeg$AVISIT %in% selvisit]))

## main analysis variable is AVALCAT1 : these have the same levels for all selected parameters
## no need to create an map dataframe for usage in layout

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

extra_args_rr <- list(
  method = "wald",
  denom = "n_df",
  .stats = c("denom", "count_unique_fraction")
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

lyt <- lyt |>
  split_rows_by(
    "PARAMCD",
    labels_var = "PARAM",
    label_pos = "topleft",
    child_labels = "default",
    split_label = "QTc Interval",
    section_div = " ",
    ## ensure only selected params are included
    split_fun = drop_split_levels
  ) |>
  split_rows_by(
    "AVISIT",
    label_pos = "topleft",
    child_labels = "default",
    split_label = "Study Visit",
    section_div = " ",
    ## ensure only the selected visits are included
    split_fun = drop_split_levels
  ) |>
  analyze(
    "AVALCAT1",
    a_freq_j,
    extra_args = extra_args_rr,
    show_labels = "hidden",
    indent_mod = 0L
  ) |>
  append_topleft(c("    Criteria, n (%)"))


result <- build_table(lyt, filtered_adeg, alt_counts_df = adsl, round_type = "sas")


################################################################################
# Post-Processing:
# - remove unwanted colcounts
################################################################################

result <- remove_col_count(result)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table
################################################################################


colwidth <- c(51, 21, 21, 21, 21)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid)
