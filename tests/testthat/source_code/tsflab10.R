library(envsetup)
library(tern)
library(dplyr)
library(tidyr)
library(lubridate)
library(rtables)
library(junco)

################################################################################
# Define script level parameters
################################################################################

tblid <- "TSFLAB10"
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
alt_ast <- "both" # Enzyme selection option: "both", "alt", or "ast"

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

# Selected visits to subset the data. set to NULL to include all visits
selvisit <- c("Cycle 02", "Cycle 05", "Cycle 04", "Cycle 03")

################################################################################
# Process markedly abnormal values from spreadsheet:
################################################################################

### Markedly Abnormal spreadsheet
markedlyabnormal_file <- read_path(datapath, "markedlyabnormal.xlsx")

markedlyabnormal_sheets <- readxl::excel_sheets(markedlyabnormal_file)

lbmarkedlyabnormal_defs <- readxl::read_excel(
  markedlyabnormal_file,
  sheet = toupper("ADDILI")
) |>
  filter(PARAMCD != "Parameter Code") |>
  filter(grepl("CRIT", VARNAME)) |>
  separate_rows(PARAMCD, sep = "\\s*\\|\\s*")

CRITs <- lbmarkedlyabnormal_defs |>
  filter(grepl("CRIT", VARNAME)) |>
  pull(VARNAME) |>
  unique()
################################################################################
# Process Data
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
    !is.na(.data[[trtvar]])
  ) |>
  filter(is.null(selvisit) | AVISIT %in% selvisit) |>
  filter(
    (alt_ast == "both" & PARCAT1 == "ALT or AST") |
      (alt_ast == "alt" & PARAMCD == "ALT") |
      (alt_ast == "ast" & PARAMCD == "AST") |
      PARCAT1 == "TBILI" |
      PARAMCD == "ALP"
  ) |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c("Xanomeline Low Dose", "Xanomeline High Dose", "Placebo")
    )
  ) |>
  mutate(
    PARCAT1 = case_when(
      PARCAT1 %in% c("ALT or AST", "ALT", "AST") ~ enzyme_label,
      TRUE ~ as.character(PARCAT1)
    ),
    DENOMFL = case_when(
      PARCAT1 == enzyme_label & ONTRTFL == "Y" ~ "Y",
      PARCAT1 == "TBILI" & ONTRTFL == "Y" ~ "Y",
      PARCAT1 == "ALP" & ONTRTFL == "Y" ~ "Y",
      TRUE ~ NA_character_
    )
  ) |>
  left_join(adsl |> select(USUBJID, colspan_trt), by = "USUBJID")

addili <- addili |>
  mutate(across(
    all_of(paste0(CRITs, "FL")),
    ~ factor(., levels = c("Y", "N"))
  )) |>
  mutate(DENOMFL = factor(DENOMFL, levels = (c("Y", NA_character_))))


xlabel_map <- lbmarkedlyabnormal_defs |>
  left_join(addili |> select(PARAMCD, PARCAT1) |> distinct(), by = c("PARAMCD")) |>
  filter(!is.na(PARCAT1)) |>
  mutate(
    var = paste0(VARNAME, "FL"),
    label = CRIT,
    value = "Y"
  ) |>
  mutate(CRIT_ = as.numeric(stringr::str_extract(label, "\\d+"))) |>
  arrange(PARCAT1, PARAMCD, CRIT_, label, VARNAME) |>
  select(PARCAT1, var, value, label) |>
  distinct()

xlabel_map1 <- xlabel_map |>
  filter(PARCAT1 == enzyme_label)
xlabel_map2 <- xlabel_map |>
  filter(PARCAT1 == "TBILI")
xlabel_map3 <- xlabel_map |>
  filter(PARCAT1 == "ALP")

################################################################################
# Layout and Build
################################################################################

extra_args <- list(
  denom = "n_df",
  .stats = "count_unique_fraction",
  na_str = "0"
)

lyt <- basic_table(show_colcounts = TRUE) |>
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  )

if (combined_colspan_trt == TRUE) {
  lyt <- lyt |> split_cols_by(trtvar, split_fun = mysplit)
} else {
  lyt <- lyt |> split_cols_by(trtvar)
}

# enzyme section
lyt <- lyt |>
  split_rows_by(
    "PARCAT1",
    parent_name = "enzyme",
    split_fun = keep_split_levels(enzyme_label),
    child_labels = "visible",
    nested = FALSE,
  ) |>
  split_rows_by(
    "DENOMFL",
    label_pos = "topleft",
    split_fun = drop_split_levels,
    child_labels = "hidden",
    split_label = " ",
    section_div = " "
  ) |>
  summarize_row_groups(
    "DENOMFL",
    cfun = a_freq_j,
    extra_args = list(
      .stats = "n_df",
      label = "Any on-treatment value",
      riskdiff = FALSE
    )
  ) |>
  analyze(
    xlabel_map1$var,
    afun = a_freq_j,
    extra_args = append(
      extra_args,
      list(
        val = "Y",
        label_map = xlabel_map1
      )
    ),
    show_labels = "hidden"
  ) |>
  split_rows_by(
    "PARCAT1",
    parent_name = "pc_av_enzyme",
    split_fun = keep_split_levels(enzyme_label),
    child_labels = "hidden"
  ) |>
  split_rows_by(
    "AVISIT",
    parent_name = "av_enzyme",
    split_fun = drop_split_levels,
    indent_mod = 1L,
    section_div = " "
  ) |>
  summarize_row_groups(
    "AVISIT",
    cfun = a_freq_j,
    extra_args = list(
      .stats = "n_df",
      riskdiff = FALSE
    )
  ) |>
  analyze(
    xlabel_map1$var,
    afun = a_freq_j,
    extra_args = append(
      extra_args,
      list(
        val = "Y",
        label_map = xlabel_map1
      )
    ),
    show_labels = "hidden"
  )

# TBILI section
lyt <- lyt |>
  split_rows_by(
    "PARCAT1",
    parent_name = "enzyme",
    split_fun = keep_split_levels("TBILI"),
    child_labels = "visible",
    nested = FALSE,
  ) |>
  split_rows_by(
    "DENOMFL",
    label_pos = "topleft",
    split_fun = drop_split_levels,
    child_labels = "hidden",
    split_label = " ",
    section_div = " "
  ) |>
  summarize_row_groups(
    "DENOMFL",
    cfun = a_freq_j,
    extra_args = list(
      .stats = "n_df",
      label = "Any on-treatment value",
      riskdiff = FALSE
    )
  ) |>
  analyze(
    xlabel_map2$var,
    afun = a_freq_j,
    extra_args = append(
      extra_args,
      list(
        val = "Y",
        label_map = xlabel_map2
      )
    ),
    show_labels = "hidden"
  ) |>
  split_rows_by(
    "PARCAT1",
    parent_name = "pc_av_enzyme",
    split_fun = keep_split_levels("TBILI"),
    child_labels = "hidden"
  ) |>
  split_rows_by(
    "AVISIT",
    parent_name = "av_enzyme",
    split_fun = drop_split_levels,
    indent_mod = 1L,
    section_div = " "
  ) |>
  summarize_row_groups(
    "AVISIT",
    cfun = a_freq_j,
    extra_args = list(
      .stats = "n_df",
      riskdiff = FALSE
    )
  ) |>
  analyze(
    xlabel_map2$var,
    afun = a_freq_j,
    extra_args = append(
      extra_args,
      list(
        val = "Y",
        label_map = xlabel_map2
      )
    ),
    show_labels = "hidden"
  )

# ALP section
lyt <- lyt |>
  split_rows_by(
    "PARCAT1",
    parent_name = "enzyme",
    split_fun = keep_split_levels("ALP"),
    child_labels = "visible",
    nested = FALSE,
  ) |>
  split_rows_by(
    "DENOMFL",
    label_pos = "topleft",
    split_fun = drop_split_levels,
    child_labels = "hidden",
    split_label = " ",
    section_div = " "
  ) |>
  summarize_row_groups(
    "DENOMFL",
    cfun = a_freq_j,
    extra_args = list(
      .stats = "n_df",
      label = "Any on-treatment value",
      riskdiff = FALSE
    )
  ) |>
  analyze(
    xlabel_map3$var,
    afun = a_freq_j,
    extra_args = append(
      extra_args,
      list(
        val = "Y",
        label_map = xlabel_map3
      )
    ),
    show_labels = "hidden"
  ) |>
  split_rows_by(
    "PARCAT1",
    parent_name = "pc_av_enzyme",
    split_fun = keep_split_levels("ALP"),
    child_labels = "hidden"
  ) |>
  split_rows_by(
    "AVISIT",
    parent_name = "av_enzyme",
    split_fun = drop_split_levels,
    indent_mod = 1L,
    section_div = " "
  ) |>
  summarize_row_groups(
    "AVISIT",
    cfun = a_freq_j,
    extra_args = list(
      .stats = "n_df",
      riskdiff = FALSE
    )
  ) |>
  analyze(
    xlabel_map3$var,
    afun = a_freq_j,
    extra_args = append(
      extra_args,
      list(
        val = "Y",
        label_map = xlabel_map3
      )
    ),
    show_labels = "hidden"
  )

result <- build_table(lyt, addili, alt_counts_df = adsl, round_type = "sas")

################################################################################
# Titles and Footnotes
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Output
################################################################################

colwidth <- c(46, 17, 17, 17, 17)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, label_width_ins = 2.4, orientation = "landscape")
