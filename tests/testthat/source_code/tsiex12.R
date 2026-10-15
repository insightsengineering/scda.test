library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)
library(stringr)
library(tidyr)


################################################################################
# Define script level parameters:
################################################################################

################################################################################
# - Define output ID and file location
# - Define treatment variable used (default=TRT01A)
# - Define population flag used (default=SAFFL)
# - Choose whether or not you want to present a combined active treatment column (default=TRUE)
# - Define how to create combined treatment columns (if required)
################################################################################

tblid <- "TSIEX12"
fileid <- write_path(opath, tblid)
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

trtvar <- "TRT01A"
popfl <- "SAFFL"
combined_colspan_trt <- TRUE
ctrl_grp <- "Placebo"

# Add flexibility to choose display level: "SOC_PT" or "PT"
# "SOC_PT" : display SOC header with PT terms nested underneath
# "PT"     : display PT terms directly (no SOC level)
display_level <- "SOC_PT" # Options: "SOC_PT", "PT"


################################################################################
# Process Data:
################################################################################

adsl <- adsl_jnj |>
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
  filter(!!rlang::sym(popfl) == "Y") |>
  select(STUDYID, USUBJID, all_of(trtvar), all_of(popfl)) |>
  create_colspan_var(
    non_active_grp = "Placebo",
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = trtvar
  )


adex1 <- adex_jnj |>
  filter(!grepl("UNSCHEDULED|BASELINE|SCREENING", AVISIT, ignore.case = TRUE)) |>
  mutate(
    AVISIT := factor(
      stringr::str_to_sentence(as.character(AVISIT)),
      levels = stringr::str_to_sentence(unique(.data[["AVISIT"]])[order(unique(.data[["AVISITN"]]))])
    )
  )


adex2 <- adex1 |>
  mutate(AVISIT = "Overall")


adex_ <- bind_rows(adex1, adex2) |>
  mutate(ROW_ID = row_number())

adex1$AVISIT <- droplevels(adex1$AVISIT)
adex_$AVISIT <- factor(
  adex_$AVISIT,
  levels = c("Overall", levels(adex1$AVISIT))
)
# Reshape Data for rtables Hierarchy

# detect existing indices dynamically
idxs <- stringr::str_extract(
  names(adex_)[stringr::str_detect(names(adex_), "^ABODSYS\\d+$")],
  "\\d+"
)


adex_not_admin <- adex_ |>
  filter(ACAT2 == "Dose not administered") |>
  mutate(
    DOSE_MOD = "Dose not administered",
    AE_FLAG = ifElse <- if_else(
      AREASOC == "Adverse Event",
      "Adverse event",
      NA_character_
    )
  )

# dynamically create AE_SOCi and AE_PTi
for (i in idxs) {
  soc_col <- paste0("ABODSYS", i)
  pt_col <- paste0("ADECOD", i)

  ae_soc <- paste0("AE_SOC", i)
  ae_pt <- paste0("AE_PT", i)

  adex_not_admin <- adex_not_admin |>
    mutate(
      !!ae_soc := if_else(
        !is.na(AE_FLAG) & !is.na(.data[[soc_col]]) & .data[[soc_col]] != "",
        .data[[soc_col]],
        NA_character_
      ),
      !!ae_pt := if_else(
        !is.na(AE_FLAG) & !is.na(.data[[pt_col]]) & .data[[pt_col]] != "",
        .data[[pt_col]],
        NA_character_
      )
    )
}


adex_adjusted <- adex_ |>
  filter(ACAT1 == "Dose adjusted") |>
  mutate(
    DOSE_MOD = "Dose adjusted",
    AE_FLAG = if_else(
      !is.na(AADJ) & AADJ == "Adverse Event",
      "Adverse event",
      NA_character_
    )
  )


# create AE_SOCi / AE_PTi with same column labels
for (i in idxs) {
  adex_adjusted <- adex_adjusted |>
    mutate(
      !!paste0("AE_SOC", i) := if_else(
        !is.na(AE_FLAG) &
          !is.na(.data[[paste0("ABODSYS", i)]]) &
          .data[[paste0("ABODSYS", i)]] != "",
        .data[[paste0("ABODSYS", i)]],
        NA_character_
      ),
      !!paste0("AE_PT", i) := if_else(
        !is.na(AE_FLAG) &
          !is.na(.data[[paste0("ADECOD", i)]]) &
          .data[[paste0("ADECOD", i)]] != "",
        .data[[paste0("ADECOD", i)]],
        NA_character_
      )
    )
}


# Combine
adex_events <- bind_rows(adex_not_admin, adex_adjusted)

soc_cols <- paste0("AE_SOC", idxs)
pt_cols <- paste0("AE_PT", idxs)

adex_no_events <- adex_ |>
  filter(!(ROW_ID %in% adex_events$ROW_ID)) |>
  mutate(
    DOSE_MOD = NA_character_,
    AE_FLAG = NA_character_,
    !!!setNames(
      rep(list(NA_character_), length(c(soc_cols, pt_cols))),
      c(soc_cols, pt_cols)
    )
  )


# Bind everything
adex_stacked <- bind_rows(adex_events, adex_no_events) |>
  select(USUBJID, STUDYID, AVISIT, AVISITN, DOSE_MOD, AE_FLAG, starts_with("AE_SOC"), starts_with("AE_PT"))


adex_stacked <- adex_stacked |>
  mutate(
    DOSE_MOD = factor(
      DOSE_MOD,
      levels = c("Dose not administered", "Dose adjusted")
    ),
    AE_FLAG = factor(
      AE_FLAG,
      levels = c("Adverse event")
    ),
    across(starts_with("AE_SOC"), factor),
    across(starts_with("AE_PT"), factor)
  )

adex_stacked <- adex_stacked |>
  tidyr::pivot_longer(
    cols = matches("^AE_(SOC|PT)\\d+$"),
    names_to = c(".value", "IDX"),
    names_pattern = "AE_(SOC|PT)(\\d+)"
  ) |>
  mutate(
    SOC = as.factor(stringr::str_to_sentence(SOC)),
    PT = as.factor(stringr::str_to_sentence(PT))
  )


# Join
ex <- adex_stacked |> left_join(adsl, by = c("USUBJID", "STUDYID"))

# Auto-detect: if all SOC values are NA, fall back to PT only
if (all(is.na(ex$SOC))) {
  display_level <- "PT"
}

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

split_combined <- if (combined_colspan_trt) {
  add_combo <- add_combo_facet(
    "Combined",
    label = "Combined",
    levels = setdiff(levels(adsl[[trtvar]]), ctrl_grp)
  )
  rm_combo_from_placebo <- cond_rm_facets(
    facets = "Combined",
    ancestor_pos = NA,
    value = " ",
    split = "colspan_trt"
  )
  make_split_fun(post = list(add_combo, rm_combo_from_placebo))
} else {
  NULL
}

extra_args1 <- list(
  .stats = "count_unique_fraction",
  denom = "n_parentdf",
  denom_by = "AVISIT"
)

lyt <- rtables::basic_table(
  top_level_section_div = " ",
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) |>
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) |>
  split_cols_by(trtvar, split_fun = split_combined) |>
  # Hierarchy Level 1: Time Point
  split_rows_by(
    "AVISIT",
    split_label = "Time Point",
    label_pos = "topleft",
    indent_mod = 0L,
    section_div = " "
  ) |>
  summarize_row_groups(
    "AVISIT",
    cfun = a_freq_j,
    extra_args = list(.stats = "n_df"),
    indent_mod = 0L
  ) |>
  split_rows_by(
    "DOSE_MOD",
    split_label = "Dose Modification",
    label_pos = "topleft",
    indent_mod = 0L
  ) |>
  summarize_row_groups(
    "DOSE_MOD",
    cfun = a_freq_j,
    extra_args = extra_args1,
    indent_mod = 0L
  ) |>
  split_rows_by(
    "AE_FLAG",
    indent_mod = 0L,
    child_labels = "hidden",
    split_fun = keep_split_levels("Adverse event")
  )

if (display_level == "SOC_PT") {
  lyt <- lyt |>
    analyze(
      vars = "AE_FLAG",
      afun = a_three_tier,
      indent_mod = 0L,
      extra_args = c(
        extra_args1,
        list(
          grp_fun = a_freq_j,
          detail_fun = a_freq_j,
          inner_var1 = "SOC",
          inner_var2 = "PT",
          drill_down_levs = "Adverse event",
          use_all_levels = FALSE
        )
      )
    ) |>
    append_topleft(c("    System Organ Class")) |>
    append_topleft(c("       Preferred Term, n (%)"))
} else if (display_level == "PT") {
  lyt <- lyt |>
    analyze(
      vars = "AE_FLAG",
      afun = a_two_tier,
      indent_mod = 0L,
      extra_args = c(
        extra_args1,
        list(
          grp_fun = a_freq_j,
          detail_fun = a_freq_j,
          inner_var = "PT",
          drill_down_levs = "Adverse event",
          use_all_levels = FALSE
        )
      )
    ) |>
    append_topleft(c("       Preferred Term, n (%)"))
}

result <- build_table(lyt, ex, alt_counts_df = adsl, round_type = "sas")


################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

# derivation of number of rows column header takes
cps <- col_paths(result)[[1]]
cps <- cps[seq(from = 1, by = 2, length.out = length(cps) / 2)]
### as showcolcounts = TRUE, 1 extra row in columns
levels_cols <- length(cps) + 1

bordmat <- junco:::make_header_bordmat(obj = result)
if (levels_cols < length(top_left(result))) {
  extra_row <- rep(0, ncol(bordmat))
  bordmat <- rbind(extra_row, bordmat)
  rownames(bordmat) <- NULL
}

################################################################################
# Convert to tbl file and output table
################################################################################


colwidth <- c(64, 21, 21, 21, 21)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, border_mat = bordmat, orientation = "portrait")
