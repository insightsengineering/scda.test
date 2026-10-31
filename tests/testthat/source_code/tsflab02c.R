library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSFLAB02c"
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

# Add Active Study Agent Combined column
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

# PARCAT1 categories to produce: CHEMISTRY -> CHM, HEMATOLOGY -> HM
parcat1_categories <- list(
  chm = "CHEMISTRY",
  hem = "HEMATOLOGY"
)

ad_domain <- "adlb"

# MCRIT indices to use (e.g. c(1, 2) for CRIT1/CRIT2, c(2, 3) for CRIT2/CRIT3)
crit_indices <- c(1, 2)

## if the option TRTEMFL needs to be added to the TLF -- ensure the same setting as in tsflab04
trtemfl <- TRUE


################################################################################
# Initial processing of data + check if table is valid for trial:
################################################################################
adlb_complete <- adlb_jnj

################################################################################
# Process markedly abnormal values from spreadsheet:
################################################################################

### Markedly Abnormal spreadsheet
markedlyabnormal_file <- read_path(datapath, "markedlyabnormal.xlsx")

markedlyabnormal_sheets <- readxl::excel_sheets(markedlyabnormal_file)

lbmarkedlyabnormal_defs <- readxl::read_excel(
  markedlyabnormal_file,
  sheet = toupper(ad_domain)
) |>
  filter(PARAMCD != "Parameter Code")

# Data Fix needed on abnormal file: This section should be removed on actual study data
# No criteria available on markedlyabnormal file — using manual values
crit_defs_data <- tibble::tribble(
  ~PARAMCD, ~VARNAME, ~CRIT,
  "ALT", "CRIT2", ">3xULN",
  "ALT", "CRIT1", ">2xULN",
  "AST", "CRIT2", ">3xULN",
  "AST", "CRIT1", ">2xULN",
  "BILI", "CRIT1", ">2xULN",
  "BILI", "CRIT2", ">3xULN",
  "CK", "CRIT1", ">2xULN",
  "CK", "CRIT2", ">3xULN",
  "CREAT", "CRIT1", ">2xULN",
  "GGT", "CRIT1", ">2xULN",
  "GGT", "CRIT2", ">3xULN",
  "HGB", "CRIT1", ">2xULN",
  "HGB", "CRIT2", ">3xULN",
  "PROT", "CRIT1", ">2xULN",
  "PROT", "CRIT2", ">3xULN",
  "SODIUM", "CRIT2", ">3xULN",
  "SODIUM", "CRIT1", ">2xULN",
  "WBC", "CRIT1", ">2xULN",
  "WBC", "CRIT2", ">3xULN"
) |>
  mutate(CRITN = as.character(as.numeric(factor(CRIT))), SEX = NA_character_)

lbmarkedlyabnormal_defs <- bind_rows(lbmarkedlyabnormal_defs, crit_defs_data)
# End of data fix: code should be removed if abnormal file have data

CRITs <- paste0("CRIT", crit_indices)
CRITsFLs <- paste0("CRIT", crit_indices, "FL")

CRITs_def <- unique(
  lbmarkedlyabnormal_defs |>
    filter(VARNAME %in% CRITs) |>
    select(PARAMCD, VARNAME, CRIT, SEX)
)

### convert dataframe into label_map that can be used with the a_freq_j afun function
xlabel_map <- CRITs_def |>
  rename(var = VARNAME, label = CRIT) |>
  select(PARAMCD, var, label)

################################################################################
# Process Data:
################################################################################
adsl <- adsl_jnj |>
  filter(.data[[popfl]] == "Y") |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c("Xanomeline Low Dose", "Xanomeline High Dose", "Placebo")
    )
  ) |>
  select(USUBJID, all_of(c(popfl, trtvar)))

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

adlb00 <- adlb_complete |>
  filter(ONTRTFL == "Y") |>
  # Filter to keep records where at least one of the specified CRIT flags is "Y"
  filter(if_any(all_of(paste0("CRIT", crit_indices, "FL")), ~ . == "Y")) |>
  select(
    USUBJID,
    PARCAT1,
    PARCAT2,
    PARCAT3,
    PARCAT3N,
    ONTRTFL,
    TRTEMFL,
    PARAM,
    PARAMCD,
    AVISITN,
    AVISIT,
    AVAL,
    all_of(paste0("CRIT", crit_indices)),
    all_of(paste0("CRIT", crit_indices, "FL"))
  ) |>
  inner_join(adsl)

#### DO NOT filter by TRTEMFL == "Y" — that removes subjects from denominator
#### Instead: set CRIT_LABEL to NA when TRTEMFL != "Y", row stays for N count
if (trtemfl) {
  adlb00 <- adlb00 |>
    mutate(
      across(
        all_of(CRITsFLs),
        ~ case_when(
          . == "Y" & !is.na(TRTEMFL) & TRTEMFL == "Y" ~ "Y",
          TRUE ~ "N"
        )
      )
    )
}

# Filter to keep records where at least one of the specified CRIT flags is "Y"
adlb00 <- adlb00 |>
  filter(if_any(all_of(paste0("CRIT", crit_indices, "FL")), ~ . == "Y"))

adlb_crit_list <- lapply(crit_indices, function(idx) {
  fl_var <- paste0("CRIT", idx, "FL")
  crit_var <- paste0("CRIT", idx)
  adlb00 |>
    filter(.data[[fl_var]] == "Y") |>
    mutate(
      CRIT_IDX = idx,
      CRIT_FL = fl_var,
      CRIT_LABEL = .data[[crit_var]]
    )
})

# Derive PARAMCD sort order: PARCAT3N -> PARAM, from deduplicated reference
paramcd_order <- adlb00 |>
  distinct(PARCAT1, PARCAT3N, PARCAT3, PARAM, PARAMCD) |>
  arrange(PARCAT1, PARCAT3N, PARCAT3, PARAM) |>
  pull(PARAMCD)

adlb_crit <- bind_rows(adlb_crit_list) |>
  mutate(PARAMCD = ordered(PARAMCD, levels = paramcd_order))
################################################################################
##### Build xlabel_map (CRIT_LABEL levels) from spreadsheet
################################################################################

xlabel_map_final <- xlabel_map |>
  filter(var %in% CRITs) |>
  inner_join(
    unique(adlb00 |> select(PARAMCD, PARAM, PARCAT1, PARCAT3N, PARCAT3)),
    by = "PARAMCD"
  ) |>
  arrange(PARCAT1, PARCAT3N, PARCAT3, PARAM, PARAMCD, var)

# Set CRIT_LABEL factor levels from spreadsheet order (scoped per PARAMCD via label_map)
crit_label_levels <- unique(xlabel_map_final$label)
adlb_crit <- adlb_crit |>
  mutate(CRIT_LABEL = factor(CRIT_LABEL, levels = crit_label_levels))

# Build label_map for a_freq_j: PARAMCD-conditional mapping of CRIT_LABEL levels
crit_label_map <- xlabel_map_final |>
  rename(value = label) |>
  select(PARAMCD, value) |>
  mutate(label = value)

################################################################################
# Define layout and build table:
################################################################################

.extra_args_rr <- list(
  method = "wald",
  denom = "n_df",
  ref_path = ref_path,
  riskdiff = TRUE,
  .stats = c("count_unique_fraction"),
  label_map = crit_label_map
)

################################################################################
# Core function to produce shell for specific parcat3 selection
################################################################################

build_result_parcat1 <- function(
  df = adlb_crit,
  PARCAT1sel = NULL,
  tblid,
  .adsl = adsl,
  map = xlabel_map_final,
  extra_args_rr = .extra_args_rr,
  .trtvar = trtvar,
  .ref_path = ref_path,
  .ctrl_grp = ctrl_grp,
  .combined_colspan_trt = combined_colspan_trt,
  .crit_indices = crit_indices
) {
  lyt_filter <- function(PARCAT1sel = NULL, map, df) {
    if (!is.null(PARCAT1sel)) {
      map <- map |>
        filter(toupper(PARCAT1) %in% toupper(PARCAT1sel))
    }

    lyt <- basic_table(show_colcounts = TRUE, colcount_format = "N=xx") |>
      split_cols_by(
        "colspan_trt",
        split_fun = trim_levels_to_map(map = colspan_trt_map)
      )

    if (.combined_colspan_trt == TRUE) {
      lyt <- lyt |> split_cols_by(.trtvar, split_fun = mysplit)
    } else {
      lyt <- lyt |> split_cols_by(.trtvar)
    }

    lyt <- lyt |>
      split_cols_by("rrisk_header", nested = FALSE) |>
      split_cols_by(
        .trtvar,
        labels_var = "rrisk_label",
        split_fun = remove_split_levels(.ctrl_grp)
      )

    lyt <- lyt |>
      split_rows_by(
        "PARAMCD",
        labels_var = "PARAM",
        split_label = "Laboratory Test",
        child_labels = "visible",
        section_div = " ",
        label_pos = "topleft",
        split_fun = drop_split_levels
      ) |>
      append_topleft("  Criteria") |>
      summarize_row_groups(
        var = "PARAMCD",
        cfun = a_freq_j,
        extra_args = list(
          .stats = "n_df",
          label = "N",
          riskdiff = FALSE
        )
      ) |>
      analyze(
        "CRIT_LABEL",
        a_freq_j,
        extra_args = extra_args_rr,
        show_labels = "hidden"
      )

    return(lyt)
  }

  if (!is.null(PARCAT1sel)) {
    df <- df |>
      filter(toupper(PARCAT1) %in% toupper(PARCAT1sel))
  }

  lyt <- lyt_filter(PARCAT1sel = PARCAT1sel, map = map, df = df)

  result <- build_table(lyt, df, alt_counts_df = .adsl, round_type = "sas")

  ################################################################################
  # Post Processing
  ################################################################################

  # Remove unwanted column counts
  result <- remove_col_count(result)

  ################################################################################
  # Set title
  ################################################################################

  result <- set_titles(result, tab_titles)

  fileid <<- write_path(opath, tblid)

  return(result)
}

################################################################################
# Define layout and build table:
################################################################################

result <- build_result_parcat1(
  PARCAT1sel = parcat1_categories[["hem"]],
  tblid = tblid
)

colwidth <- c(34, 23, 23, 23, 23, 30, 33)

fileid <- write_path(opath, tblid)

tt_to_tlgrtf(
  # colwidths = colwidth,
  result,
  file = fileid,
  orientation = "landscape"
)
