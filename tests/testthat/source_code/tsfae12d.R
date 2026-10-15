library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)
library(stringr)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSFAE12d"
fileid <- write_path(opath, tblid)
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


trtvar <- "TRT01A"
popfl <- "SAFFL"
combined_colspan_trt <- TRUE
risk_diff <- FALSE
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
  filter(!!rlang::sym(popfl) == "Y") |>
  select(STUDYID, USUBJID, all_of(trtvar), all_of(popfl)) |>
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

adae <- adae_jnj |>
  mutate(
    AEBODSYS = case_when(
      AEBODSYS == "" ~ "Uncoded",
      .default = AEBODSYS
    ),
    AEDECOD = case_when(
      AEDECOD == "" ~ paste0("Uncoded: ", AETERM),
      .default = AEDECOD
    ),
    AERELTOT = ifelse(AEREL %in% c("RELATED", "NOT RELATED"), stringr::str_to_title(AEREL), NA)
  ) |>
  filter(TRTEMFL == "Y" & !is.na(AERELTOT)) |>
  select(USUBJID, TRTEMFL, AEBODSYS, AEDECOD, AEREL, AERELTOT)

adaetot <- adae |>
  mutate(
    AERELTOT = "Total"
  )

adaeall <- bind_rows(adae, adaetot) |>
  mutate(AERELTOT = factor(as.character(AERELTOT), levels = c("Total", "Not Related", "Related")))


adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == "Placebo", " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

# join data together
ae <- adaeall |> inner_join(adsl, by = c("USUBJID"))

ae$spanheader <- factor(ifelse(ae$AERELTOT == "Total", " ", "Relationship"), levels = c(" ", "Relationship"))

adsl1 <- adsl |>
  mutate(AERELTOT = "Total")

adsl <- adsl1 |>
  mutate(AERELTOT = factor(as.character(AERELTOT), levels = c("Total", "Not Related", "Related")))
adsl$spanheader <- factor(ifelse(adsl$AERELTOT == "Total", " ", "Relationship"), levels = c(" ", "Relationship"))


if (risk_diff == TRUE) {
  adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
  adsl$rrisk_label <- paste(adsl[[trtvar]], paste("vs", ctrl_grp))
}


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

extra_args_1 <- list(
  denom = "N_colgroup",
  riskdiff = FALSE,
  .stats = c("count_unique_fraction"),
  colgroup = trtvar,
  ref_path = ref_path
)


lyt <- basic_table(
  top_level_section_div = " ",
  colcount_format = "N=xx"
) |>
  split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map))

if (combined_colspan_trt == TRUE) {
  lyt <- lyt |>
    split_cols_by(trtvar, split_fun = mysplit, show_colcounts = TRUE)
} else {
  lyt <- lyt |>
    split_cols_by(trtvar, show_colcounts = TRUE)
}

lyt <- lyt |>
  split_cols_by("spanheader", split_fun = trim_levels_in_group("AERELTOT")) |>
  split_cols_by("AERELTOT", show_colcounts = FALSE) |>
  analyze(
    "TRTEMFL",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = append(extra_args_1, list(label = "Subjects with >=1 AE", val = "Y"))
  ) |>
  split_rows_by(
    "AEBODSYS",
    split_label = "System Organ Class",
    split_fun = trim_levels_in_group("AEDECOD"),
    label_pos = "topleft",
    section_div = c(" "),
    nested = FALSE
  ) |>
  summarize_row_groups("AEBODSYS", cfun = a_freq_j, extra_args = extra_args_1) |>
  analyze("AEDECOD", afun = a_freq_j, extra_args = extra_args_1) |>
  append_topleft("  Preferred Term, n (%)")


result <- build_table(lyt, ae, alt_counts_df = adsl, round_type = "sas")

if (length(adae$TRTEMFL) != 0) {
  result <- sort_at_path(
    result,
    c("root", "AEBODSYS"),
    scorefun = jj_complex_scorefun(
      colpath = c("colspan_trt", "Active Study Agent", trtvar, "Combined", "spanheader", " ", "AERELTOT", "Total")
    )
  )
  result <- sort_at_path(
    result,
    c("root", "AEBODSYS", "*", "AEDECOD"),
    scorefun = jj_complex_scorefun(
      colpath = c("colspan_trt", "Active Study Agent", trtvar, "Combined", "spanheader", " ", "AERELTOT", "Total")
    )
  )
}

################################################################################
## Remove the N=xx column headers for the risk difference columns
################################################################################

result <- remove_col_count(result)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table
################################################################################


colwidth <- c(64, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "landscape")
