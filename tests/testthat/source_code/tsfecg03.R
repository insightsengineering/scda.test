library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)
library(haven)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSFECG03"
fileid <- write_path(opath, tblid)
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

popfl <- "SAFFL"
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"

# Add Active Study Agent Combined column?
combined_colspan_trt <- TRUE

## selection of QTC parameters
selparamcd <- c("QTCBAG", "QTCFAG", "QTCS", "QTCLAG")


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

adeg <- adeg_jnj

### available QT Interval parameters in study
selparamcd <- intersect(selparamcd, unique(adeg$PARAMCD))

adeg <- adeg |>
  filter(
    !is.na(USUBJID),
    .data[[popfl]] == "Y",
    !is.na(.data[[trtvar]]),
    PARAMCD %in% selparamcd,
    ### Maximum On-treatment
    ### note: by filter ANL03FL, this table is restricted to On-treatment values, per definition of ANL03FL
    ANL03FL == "Y"
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
  select(
    STUDYID,
    USUBJID,
    PARAM,
    PARAMN,
    PARAMCD,
    CHGCAT1,
    ANL03FL
  )

# restrict to these - ordered by PARAMN
selparamcd <- as.character(
  adeg |> arrange(PARAMN) |> pull(PARAMCD) |> unique()
)

adeg <- adeg |>
  mutate(
    PARAMCD := factor(PARAMCD, levels = selparamcd),
    PARAM := factor(PARAM)
  ) |>
  inner_join(adsl, by = c("STUDYID", "USUBJID"))

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
    split_label = "QTc Interval",
    label_pos = "topleft",
    split_fun = drop_split_levels,
    section_div = " "
  ) |>
  analyze(
    c("CHGCAT1"),
    a_freq_j,
    extra_args = extra_args_rr,
    show_labels = "hidden",
    indent_mod = 0L
  ) |>
  append_topleft("  Criteria, n (%)")

result <- build_table(lyt, adeg, alt_counts_df = adsl, round_type = "sas")


################################################################################
# Post-Processing:
# - remove unwanted colcounts
# - adjust Combined column N if combined_colspan_trt = TRUE
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
