library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)
library(haven)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSIEX05"
fileid <- write_path(opath, tblid)
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

trtvar <- "TRT01A"
popfl <- "SAFFL"

# Specifies the study agent to filter exposure data; leave empty (NULL or "") to include all treatment groups
study_agent <- "XANOMELINE"

# add required levels for dose
dose_level <- c(7.5, 15)

# Set to FALSE to hide dose level breakdown rows in the layout
show_dose_level <- TRUE

# Set Parameters for Dose interruptions /Dose skipped (and not made up) /Dose delay
# Dose interuption then use ANL03FL == "Y" and INTCAT variable
# Dose skipped (and not made up) then use flag ANL04FL == "Y"  and SKPCAT
# Dose delay then use ADOSDLY == "Y" and DLYCAT
analysis_flag <- "ANL03FL"
analysis_variable <- "INTCAT"
analysis_label <- 'dose interruptions'

# Threshold for dose delay; adjust according to study
dly_threshold <- 3

trt_levels <- if (!is.null(study_agent) && nzchar(study_agent)) {
  c("Xanomeline Low Dose", "Xanomeline High Dose")
} else {
  c("Xanomeline Low Dose", "Xanomeline High Dose", "Placebo")
}

non_active_grp <- if (!is.null(study_agent) && nzchar(study_agent)) NULL else "Placebo"

combined_colspan_trt <- TRUE

if (combined_colspan_trt == TRUE) {
  add_combo <- add_combo_facet(
    "Combined",
    label = "Combined",
    levels = c("Xanomeline High Dose", "Xanomeline Low Dose")
  )
  if (is.null(non_active_grp)) {
    mysplit <- make_split_fun(post = list(add_combo))
  } else {
    rm_combo_from_placebo <- cond_rm_facets(
      facets = "Combined",
      ancestor_pos = NA,
      value = " ",
      split = "colspan_trt"
    )
    mysplit <- make_split_fun(post = list(add_combo, rm_combo_from_placebo))
  }
}
################################################################################
# Process Data:
################################################################################

adsl <- adsl_jnj |>
  filter(.data[[popfl]] == "Y") |>
  select(STUDYID, USUBJID, all_of(c(trtvar, popfl))) |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = trt_levels
    )
  )

if (TRUE) {
  adsl <- adsl |>
    create_colspan_var(
      non_active_grp = if (is.null(non_active_grp)) character(0) else non_active_grp,
      non_active_grp_span_lbl = " ",
      active_grp_span_lbl = "Active Study Agent",
      colspan_var = "colspan_trt",
      trt_var = trtvar
    )
}

adex <- adex_jnj |>
  select(
    STUDYID,
    USUBJID,
    all_of(c(trtvar, popfl)),
    starts_with("ANL"),
    ASCHDOSE,
    ASCHDOSU,
    ADOSDLY,
    INTCAT,
    SKPCAT,
    DLYCAT,
    ATRT
  ) |>
  filter(.data[[popfl]] == "Y", if (!is.null(study_agent) && nzchar(study_agent)) ATRT == study_agent else TRUE) |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = trt_levels
    )
  )

adsl_join <- adsl |> select(STUDYID, USUBJID, colspan_trt)

ex <- adex |>
  inner_join(adsl_join, by = c("STUDYID", "USUBJID")) |>
  mutate(
    aschdose_chr = as.character(ASCHDOSE),
    aschdosu_val = dplyr::first(na.omit(ASCHDOSU)),
    ASCHDOSE = factor(
      if_else(ANL01FL == "Y", aschdose_chr, NA_character_),
      levels = dose_level,
      labels = paste("To", dose_level, unique(aschdosu_val))
    ),
    ASCHDOSE2 = factor(
      if_else(ANL02FL == "Y", aschdose_chr, NA_character_),
      levels = dose_level,
      labels = paste("To", dose_level, unique(aschdosu_val))
    ),
    # Use flag ANL04FL == "Y" for Dose Skipped OR  ADOSDLY == "Y" for Dose Delay
    ANL_SEC3_FL = if_else(.data[[analysis_flag]] == "Y", "Y", "N"),
    !!rlang::sym(paste0(analysis_variable, "1")) := factor(
      if_else(
        ANL_SEC3_FL == "Y" & (!is.na(.data[[analysis_variable]]) & as.numeric(.data[[analysis_variable]]) == 1),
        "Y",
        "N"
      ),
      levels = c("Y", "N")
    ),
    !!rlang::sym(paste0(analysis_variable, "2")) := factor(
      if_else(
        ANL_SEC3_FL == "Y" & (!is.na(.data[[analysis_variable]]) & as.numeric(.data[[analysis_variable]]) == 2),
        "Y",
        "N"
      ),
      levels = c("Y", "N")
    ),
    !!rlang::sym(paste0(analysis_variable, "3")) := factor(
      if_else(
        ANL_SEC3_FL == "Y" &
          (!is.na(.data[[analysis_variable]]) & as.numeric(.data[[analysis_variable]]) >= dly_threshold),
        "Y",
        "N"
      ),
      levels = c("Y", "N")
    )
  ) |>
  select(-aschdose_chr, -aschdosu_val)

if (TRUE) {
  colspan_trt_map <- if (!is.null(non_active_grp)) {
    create_colspan_map(
      adsl,
      non_active_grp = non_active_grp,
      non_active_grp_span_lbl = " ",
      active_grp_span_lbl = "Active Study Agent",
      colspan_var = "colspan_trt",
      trt_var = trtvar
    )
  } else {
    data.frame(
      colspan_trt = rep("Active Study Agent", nlevels(adsl[[trtvar]])),
      TRT01A = levels(adsl[[trtvar]])
    ) |>
      setNames(c("colspan_trt", trtvar))
  }
}

################################################################################
# Define layout and build table:
################################################################################

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx",
  top_level_section_div = " "
) |>
  append_topleft("Dose Modification, n (%)")

if (TRUE) {
  lyt <- lyt |>
    split_cols_by(
      "colspan_trt",
      split_fun = trim_levels_to_map(map = colspan_trt_map)
    )
}

if (combined_colspan_trt == TRUE) {
  lyt <- lyt |> split_cols_by(trtvar, split_fun = mysplit)
} else {
  lyt <- lyt |> split_cols_by(trtvar)
}

lyt <- lyt |>
  # ── Section 1: Prescribed dose reductions ──────────────────────────────────
  analyze(
    "ANL01FL",
    var_labels = "Prescribed dose reductions",
    afun = a_freq_j,
    extra_args = list(
      val = "Y",
      label = ">=1 dose reduction",
      denom = "n_df",
      .stats = "count_unique_fraction"
    ),
    show_labels = "visible",
    indent_mod = 0L
  )

if (show_dose_level) {
  lyt <- lyt |>
    analyze(
      "ASCHDOSE",
      afun = a_freq_j,
      extra_args = list(
        denom = "N_col",
        .stats = "count_unique_fraction"
      ),
      show_labels = "hidden",
      indent_mod = 2L,
      nested = TRUE
    )
}

lyt <- lyt |>
  # ── Section 2: Prescribed dose reductions due to AEs ───────────────────────
  analyze(
    "ANL02FL",
    var_labels = "Prescribed dose reductions due to AEs",
    afun = a_freq_j,
    extra_args = list(
      val = "Y",
      label = ">=1 dose reduction",
      denom = "n_df",
      .stats = "count_unique_fraction"
    ),
    show_labels = "visible",
    indent_mod = 0L,
    nested = FALSE
  )

if (show_dose_level) {
  lyt <- lyt |>
    analyze(
      "ASCHDOSE2",
      afun = a_freq_j,
      extra_args = list(
        denom = "N_col",
        .stats = "count_unique_fraction"
      ),
      show_labels = "hidden",
      indent_mod = 2L,
      nested = TRUE,
      table_names = "aschdose2"
    )
}

lyt <- lyt |>
  # ── Section 3: Dose interruptions / dose skipped / dose delay ──────────────
  analyze(
    "ANL_SEC3_FL",
    var_labels = stringr::str_to_sentence(analysis_label), #/Dose skipped (and not made up)/Dose delay
    afun = a_freq_j,
    extra_args = list(
      val = "Y",
      label = paste(">=1", analysis_label), #/dose skipped (and not made up)/dose delay
      denom = "n_df",
      .stats = "count_unique_fraction"
    ),
    show_labels = "visible",
    indent_mod = 0L,
    nested = FALSE
  ) |>
  analyze(
    paste0(analysis_variable, "1"),
    afun = a_freq_j,
    extra_args = list(
      val = "Y",
      label = "1",
      denom = "N_col",
      .stats = "count_unique_fraction"
    ),
    show_labels = "hidden",
    indent_mod = 2L,
    nested = TRUE
  ) |>
  analyze(
    paste0(analysis_variable, "2"),
    afun = a_freq_j,
    extra_args = list(
      val = "Y",
      label = "2",
      denom = "N_col",
      .stats = "count_unique_fraction"
    ),
    show_labels = "hidden",
    indent_mod = 2L,
    nested = TRUE
  ) |>
  analyze(
    paste0(analysis_variable, "3"),
    afun = a_freq_j,
    extra_args = list(
      val = "Y",
      label = paste0(">=", dly_threshold),
      denom = "N_col",
      .stats = "count_unique_fraction"
    ),
    show_labels = "hidden",
    indent_mod = 2L,
    nested = TRUE
  )

result <- build_table(lyt, ex, alt_counts_df = adsl, round_type = "sas")

################################################################################
# Add pruning steps
# Add titles and footnotes:
################################################################################
result <- prune_table(
  result,
  prune_func = function(tt) {
    if (obj_name(tt) %in% c("ASCHDOSE", "ASCHDOSE2", paste0(analysis_variable, 1:3))) {
      prune_empty_level(tt)
    } else {
      FALSE
    }
  }
)

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table
################################################################################


colwidth <- c(62, 21, 21, 21)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "portrait")
