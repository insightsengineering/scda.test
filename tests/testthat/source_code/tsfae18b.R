library(envsetup)
library(dplyr)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSFAE18b"
fileid <- write_path(opath, tblid)
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")
popfl <- "SAFFL"
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"
combined_colspan_trt <- TRUE
risk_diff <- TRUE

################################################################################
# Process data:
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
  mutate(
    colspan_trt = factor(
      if_else(.data[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
      levels = c("Active Study Agent", " ")
    )
  ) |>
  mutate(rrisk_header = "Risk Difference (%) (95% CI)") |>
  mutate(rrisk_label = factor(paste(.data[[trtvar]], "vs", ctrl_grp))) |>
  select(
    USUBJID,
    all_of(trtvar),
    DIABETFL,
    colspan_trt,
    rrisk_header,
    rrisk_label
  )

adagocmq <- adagocmq_jnj |>
  filter(.data[[popfl]] == "Y", ACAT1 == "Hypoglycemia", ANL01FL == "Y") |>
  select(USUBJID, ATERMN, ATERM) |>
  right_join(adsl) |>
  mutate(
    DIABETFL = if_else(
      DIABETFL == "Y",
      "History of diabetes",
      "No history of diabetes"
    ),
    DIABETFL = factor(
      DIABETFL,
      levels = c(
        "No history of diabetes",
        "History of diabetes"
      )
    )
  ) |>
  mutate(
    criterion = factor(if_else(
      ATERMN %in% 31:34,
      "Subjects with >=1 algorithmic criterion",
      " "
    ))
  ) |>
  mutate(
    ATERM = factor(
      ATERM,
      unique(c(
        "Any hypoglycemia OCMQ narrow term",
        if_else(
          any(grepl("mg/dL", ATERM)),
          "Plasma glucose <54 mg/dL",
          "Plasma glucose <3.0 mmol/L"
        ),
        if_else(
          any(grepl("mg/dL", ATERM)),
          "Hypoglycemia term + plasma glucose <70 mg/dL",
          "Hypoglycemia term + plasma glucose <3.885 mmol/L"
        ),
        if_else(
          any(grepl("mg/dL", ATERM)),
          ">=2 hypoglycemia terms + >=2 episodes of plasma glucose <70 mg/dL",
          ">=2 hypoglycemia terms + >=2 episodes of plasma glucose <3.885 mmol/L"
        ),
        levels(ATERM)
      ))
    )
  )

################################################################################
# Define layout and build table:
################################################################################

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)

if (combined_colspan_trt) {
  # Set up levels and label for the required combined columns
  add_combo <- add_combo_facet(
    "Combined",
    label = "Combined",
    levels = c("Xanomeline High Dose", "Xanomeline Low Dose")
  )

  # Choose if any facets need to be removed
  # e.g remove the combined column for placebo
  rm_combo_from_placebo <- cond_rm_facets(
    facets = "Combined",
    ancestor_pos = NA,
    value = " ",
    split = "colspan_trt"
  )

  mysplit <- make_split_fun(post = list(add_combo, rm_combo_from_placebo))
}

label_map <- tibble(
  value = levels(adagocmq[["ATERM"]])[1:4],
  label = value
)

label_map[["label"]][3] <- paste0(
  gsub("term", "term~[super a]", label_map[["label"]][3]),
  "~[super b]"
)

label_map[["label"]][4] <-
  gsub("terms", "terms~[super a]", label_map[["label"]][4])

lyt <-
  basic_table(
    show_colcounts = TRUE,
    colcount_format = "N=xx"
  ) |>
  append_topleft(c("Population", " ", "  Algorithmic OCMQ Criterion, n (%)")) |>
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  )

if (combined_colspan_trt) {
  lyt <- lyt |>
    split_cols_by(trtvar, split_fun = mysplit)
} else {
  lyt <- lyt |>
    split_cols_by(trtvar)
}

if (risk_diff) {
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
    "DIABETFL",
    split_fun = add_overall_level("All subjects"),
    section_div = " "
  ) |>
  summarize_row_groups(
    "DIABETFL",
    cfun = a_freq_j,
    extra_args = list(riskdiff = FALSE, .stats = "count_unique_fraction")
  ) |>
  split_rows_by(
    "criterion",
    split_fun = keep_split_levels("Subjects with >=1 algorithmic criterion")
  ) |>
  summarize_row_groups(
    "criterion",
    cfun = a_freq_j,
    extra_args = list(
      method = "wald",
      .stats = "count_unique_fraction",
      ref_path = ref_path
    )
  ) |>
  analyze(
    "ATERM",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      method = "wald",
      .stats = "count_unique_fraction",
      ref_path = ref_path,
      val = levels(adagocmq[["ATERM"]])[1:4],
      label_map = label_map
    )
  )

result <- build_table(lyt, adagocmq, alt_counts_df = adsl, round_type = "sas")

## Remove the N=xx column headers for the risk difference columns
result <- remove_col_count(result)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table:
################################################################################


colwidth <- c(64, 23, 23, 25, 23, 26, 24)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "landscape")
