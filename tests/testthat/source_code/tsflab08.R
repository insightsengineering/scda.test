library(envsetup)
library(tern)
library(dplyr)
library(tidyr)
library(lubridate)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSFLAB08"
fileid <- write_path(opath, tblid)
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

# Population flag variable (default=SAFFL).
popfl <- "SAFFL"
# Actual treatment variable (default=TRT01A).
trtvar <- "TRT01A"

ctrl_grp <- "Placebo"

alt_ast <- "both" # Enzyme selection: "both", "alt", or "ast"

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


################################################################################
# Process Data:
################################################################################

enzyme_label <- switch(alt_ast,
  alt = "ALT",
  ast = "AST",
  "ALT or AST"
)

adsl <- adsl_jnj |>
  filter(.data[[popfl]] == "Y") |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c("Xanomeline Low Dose", "Xanomeline High Dose", "Placebo")
    )
  ) |>
  select(STUDYID, USUBJID, all_of(trtvar))

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

addili <- addili_jnj |>
  filter(
    .data[[popfl]] == "Y",
    PARAMCD == "HDILI",
    !is.na(.data[[trtvar]])
  ) |>
  mutate(
    AVALCAT = reorder(factor(paste0(AVALCAT1, " (", AVALCAT2, ")")), AVALCA1N)
  ) |>
  left_join(adsl |> select(USUBJID, colspan_trt), by = "USUBJID")


# Labels
quad_levels <- levels(addili$AVALCAT)
crit_levels <- c(
  unique(as.character(na.omit(addili$AVALC[addili$AVALCA1N == 1]))),
  unique(as.character(na.omit(addili$AVALC[addili$AVALCA1N == 2]))),
  unique(as.character(na.omit(addili$AVALC[addili$AVALCA1N == 3]))),
  unique(as.character(na.omit(addili$AVALC[addili$AVALCA1N == 4])))
)

hdili_label <- paste0(unique(as.character(na.omit(addili$CRIT1))), "~[super a]")
if (alt_ast != "both") {
  crit_levels <- gsub("ALT or AST|ALT and AST", enzyme_label, crit_levels)
  hdili_label <- gsub("ALT or AST", enzyme_label, hdili_label)
}

################################################################################
# Define layout and build table:
################################################################################

extra_args <- list(
  denom = "n_parentdf",
  denom_by = "PARAMCD",
  .stats = "count_unique_fraction"
)

lyt <- basic_table(show_colcounts = TRUE, colcount_format = "N=xx") |>
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  )

if (combined_colspan_trt == TRUE) {
  lyt <- lyt |> split_cols_by(trtvar, split_fun = mysplit)
} else {
  lyt <- lyt |> split_cols_by(trtvar)
}

lyt <- lyt |>
  append_topleft(c(" ", "Quadrant, n (%)")) |>
  split_rows_by(
    "PARAMCD",
    split_fun = keep_split_levels("HDILI"),
    nested = FALSE,
    child_labels = "hidden"
  ) |>
  analyze(
    "PARAMCD",
    afun = a_freq_j,
    extra_args = list(
      val = "HDILI",
      .stats = "count",
      label = paste0(
        "Subjects with on-treatment ",
        enzyme_label,
        " and TBILI values"
      ),
      extrablankline = TRUE
    ),
    show_labels = "hidden"
  ) |>
  split_rows_by(
    "AVALCAT",
    parent_name = "q1",
    split_fun = keep_split_levels(quad_levels[1]),
    nested = FALSE
  ) |>
  analyze(
    "PARAMCD",
    afun = a_freq_j,
    extra_args = append(
      extra_args,
      list(val = "HDILI", label = crit_levels[1])
    ),
    show_labels = "hidden"
  ) |>
  analyze(
    "CRIT1FL",
    afun = a_freq_j,
    extra_args = append(extra_args, list(val = "Y", label = hdili_label)),
    show_labels = "hidden",
    indent_mod = 1L
  ) |>
  split_rows_by(
    "AVALCAT",
    parent_name = "q2-q4",
    split_fun = keep_split_levels(quad_levels[2:4]),
    nested = FALSE
  ) |>
  analyze(
    "AVALC",
    afun = a_freq_j,
    extra_args = append(extra_args, list(drop_levels = TRUE)),
    show_labels = "hidden"
  )

result <- build_table(
  lyt,
  addili,
  alt_counts_df = adsl,
  round_type = "sas"
)

################################################################################
# Titles and Footnotes
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Output
################################################################################


colwidth <- c(64, 21, 21, 21, 21)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, label_width_ins = 2.4, orientation = "landscape")
