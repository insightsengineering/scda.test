library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

################################################################################
# - Define output ID and file location
# - Define treatment variable used (default=TRT01A)
# - Define population flag used (default=SAFFL)
# - Choose whether or not you want to present a combined active treatment column (default=TRUE)
# - Choose whether or not you want to present the risk difference columns (default=TRUE)
# - Choose which risk difference method you would like (default=Wald)
# - Define what the control treatment group is for your study (e.g Placebo)
# - Define how to create combined treatment columns (if required)
################################################################################

tblid <- "TSFAE07c"
fileid <- write_path(opath, tblid)
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)


trtvar <- "TRT01A"
popfl <- "SAFFL"
special_interest_var <- c("CQ01NAM", "CQ02NAM", "CQ03NAM")
combined_colspan_trt <- TRUE
risk_diff <- TRUE
rr_method <- "wald"
ctrl_grp <- "Placebo"

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
  select(STUDYID, USUBJID, all_of(trtvar), all_of(popfl))

adae <- adae_jnj |>
  filter(TRTEMFL == "Y" & if_any(all_of(special_interest_var), ~ !is.na(.))) |>
  select(USUBJID, TRTEMFL, all_of(special_interest_var))

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == "Placebo", " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

if (risk_diff == TRUE) {
  adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
  adsl$rrisk_label <- paste(adsl[[trtvar]], paste("vs", ctrl_grp))
}

# join data together
ae <- adae |> right_join(adsl, by = c("USUBJID"))

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)

################################################################################
# Define layout and build table:
################################################################################

extra_args_rr2 <- list(
  denom = "n_altdf",
  riskdiff = TRUE,
  ref_path = ref_path,
  method = "wald",
  .stats = c("count_unique_fraction"),
  .formats = c(
    "rr_ci_3d" = jjcsformat_xx("xx.x (xx.x, xx.x)"),
    "n_df" = "xx",
    "aaa" = "xx"
  )
)
lyt <- basic_table(
  top_level_section_div = " ",
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

if (risk_diff == TRUE) {
  lyt <- lyt |>
    split_cols_by("rrisk_header", nested = FALSE) |>
    split_cols_by(
      trtvar,
      labels_var = "rrisk_label",
      split_fun = remove_split_levels("Placebo")
    )
}

lyt <- lyt |>
  analyze(
    special_interest_var,
    afun = a_freq_j,
    section_div = " ",
    show_labels = "hidden",
    extra_args = extra_args_rr2
  ) |>
  append_topleft("AE of Interest Assessment, n (%)")

result <- build_table(lyt, ae, alt_counts_df = adsl, round_type = "sas")

## Remove the N=xx column headers for the risk difference columns
result <- remove_col_count(result)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table
################################################################################


colwidth <- c(29, 21, 21, 21, 21, 28, 28)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "landscape")
