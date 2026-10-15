library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)
library(stringr)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSFAE01b"
fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01A"
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)
combined_colspan_trt <- TRUE
risk_diff <- TRUE
ctrl_grp <- "Placebo"
combination_trt <- FALSE # Provide whether this is a combination treatments study

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

if (combination_trt) {
  comb_trtvars <- c("AEDRGS1", "AEDRGS2") # Provide the variables containing combination treatment information in sequence

  n_comb_trt <- length(comb_trtvars)

  comb_relvars <- paste0("AERELS", seq_len(n_comb_trt)) # This will create a variable list like AERELS1, AERELS2 etc
  comb_acnvars <- paste0("AEACNS", seq_len(n_comb_trt)) # This will create a variable list like AEACNS1, AEACNS2 etc
  comb_suffix <- seq_len(n_comb_trt)
} else if (!combination_trt) {
  comb_trtvars <- trtvar
  comb_relvars <- "AEREL"
  comb_acnvars <- "AEACN"
  comb_suffix <- ""
}

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
  create_colspan_var(
    non_active_grp = ctrl_grp,
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = trtvar
  ) |>
  select(
    USUBJID,
    !!rlang::sym(popfl),
    !!rlang::sym(trtvar),
    colspan_trt
  )

if (risk_diff == TRUE) {
  adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
  adsl$rrisk_label <- paste(adsl[[trtvar]], paste("vs", ctrl_grp))
}

adae <- adae_jnj |>
  filter(TRTEMFL == "Y") |>
  select(
    USUBJID,
    all_of(comb_trtvars),
    all_of(comb_acnvars),
    all_of(comb_relvars),
    TRTEMFL,
    AESER,
    AESMIE,
    AETOXGR,
    AETOXGRN,
    AEACN,
    AEOUT,
    AOCTIFL,
    TRDISCFL,
    AESDTH,
    AESLIFE,
    AESHOSP,
    AESDISAB,
    AESCONG,
    AESMIE
  ) |>
  group_by(USUBJID) |>
  mutate(maxtox = AETOXGRN[which(AOCTIFL == "Y")][1], maxtoxcm = ifelse(maxtox >= "3", "Y", NA)) |>
  ungroup() |>
  mutate(maxtox = ifelse(is.na(maxtox), "Missing", paste("Grade", maxtox))) |>
  mutate(
    maxtox = factor(
      maxtox,
      levels = c(
        "Grade 1",
        "Grade 2",
        "Grade 3",
        "Grade 4",
        "Grade 5",
        "Missing"
      )
    )
  ) |>
  (\(z) {
    df <- z

    df |>
      mutate(
        !!!setNames(
          lapply(comb_relvars, function(x) {
            if_else(
              df$AESER == "Y" & df[[x]] == "RELATED",
              "Y",
              NA_character_
            )
          }),
          paste0("Rel_SAEs", comb_suffix)
        ),
        !!!setNames(
          lapply(comb_relvars, function(x) {
            if_else(
              df$AEOUT == "FATAL" & df[[x]] == "RELATED",
              "Y",
              NA_character_
            )
          }),
          paste0("Rel_AE_Death", comb_suffix)
        )
      )
  })() |>
  mutate(
    across(
      all_of(comb_acnvars),
      ~ {
        levels(.) <- str_to_sentence(levels(.))
        .
      }
    )
  ) |>
  select(
    -any_of(trtvar)
  )


adae <- inner_join(adae, adsl, by = c("USUBJID"))

################################################################################
# Define layout and build table:
################################################################################

if (combination_trt) {
  comb_rel_ae_labels <- paste(sapply(comb_trtvars, \(x) unique(adae[[x]])[1]), "related AEs")

  comb_rel_sae_vars <- paste0("Rel_SAEs", seq_len(n_comb_trt))
  comb_rel_sae_labels <- paste(sapply(comb_trtvars, \(x) unique(adae[[x]])[1]), "related SAEs")

  comb_rel_aedth_vars <- paste0("Rel_AE_Death", seq_len(n_comb_trt))
  comb_rel_aedth_labels <- paste(sapply(comb_trtvars, \(x) unique(adae[[x]])[1]), "related AEs leading to death")

  comb_acn_labels <- paste(
    "AE leading to dose modification of study treatment",
    sapply(comb_trtvars, \(x) tolower(unique(adae[[x]])[1])),
    "~[super b,c]"
  )
} else if (!combination_trt) {
  comb_rel_ae_labels <- "Related AEs"

  comb_rel_sae_vars <- "Rel_SAEs"
  comb_rel_sae_labels <- "Related SAEs"

  comb_rel_aedth_vars <- "Rel_AE_Death"
  comb_rel_aedth_labels <- "Related AEs leading to death"

  comb_acn_labels <- "AE leading to dose modification of study treatment~[super b,c]"
}


colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

##################################
# Check the levels of AEACN
##################################

aeacn_levels <- adae |>
  select(all_of(comb_acnvars)) |>
  lapply(levels) |>
  unlist(use.names = FALSE) |>
  unique() |>
  str_to_sentence()

# Here we are not considering "Drug Withdrawn", "Dose Not Changed", "Not Applicable"
excl_aeacn_levels <- c("Drug withdrawn", "Dose not changed", "Not applicable", "Unknown")
dosemod_lvls <- aeacn_levels[!(aeacn_levels %in% excl_aeacn_levels)]


## rearrange levels for AEACN_DECODE

newsort_AEACN <- unique(c(
  "Drug interrupted",
  "Dose reduced",
  "Dose rate reduced",
  "Dose increased",
  aeacn_levels
))


adae <- adae |>
  mutate(
    across(
      all_of(comb_acnvars),
      ~ forcats::fct_relevel(., newsort_AEACN)
    )
  )


## mapping table for label updates

dosemod_lblmap <- tibble(value = dosemod_lvls, label = dosemod_lvls) |>
  mutate(
    label = case_when(
      value == "Dose increased" ~ label,
      value == "Dose reduced" ~ "Reduction of study treatment",
      value == "Drug interrupted" ~ "Interruption of study treatment",
      TRUE ~ label
    )
  )


dosemod_spf <- make_combo_splitfun(
  nm = "modified",
  label = "AE leading to dose modification of study",
  levels = c("Dose reduced", "Dose increased", "Drug interrupted", "Dose rate reduced")
)
aesevall_spf <- make_combo_splitfun(
  nm = "AESEV_ALL",
  label = "Worst toxicity grade~[super a]",
  levels = NULL
)

aeserall_spf <- make_combo_splitfun(
  nm = "AESER_ALL",
  label = "SAE classification~[super c]",
  levels = "Y"
)

################################################################################
# Define layout and build table:
################################################################################

rr_method <- "wald"
ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)
extra_args_rr <- list(
  method = rr_method,
  ref_path = ref_path,
  .stats = c("count_unique_fraction")
)

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx",
  top_level_section_div = " "
) |>
  append_topleft(c(" ", " ", "Event, n (%)")) |>
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
  split_rows_by(
    "TRTEMFL",
    split_fun = keep_split_levels("Y"),
    section_div = " "
  ) |>
  summarize_row_groups(
    "TRTEMFL",
    cfun = a_freq_j,
    extra_args = list(
      label = "AEs",
      method = rr_method,
      ref_path = ref_path,
      .stats = c("count_unique_fraction")
    )
  ) |>
  analyze(
    "AESER",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = append(
      extra_args_rr,
      list(label = "SAEs", val = "Y", NULL)
    )
  )

for (i in seq_along(comb_relvars)) {
  lyt <- lyt |>
    analyze(
      comb_relvars[i],
      afun = a_freq_j,
      show_labels = "hidden",
      extra_args = append(
        extra_args_rr,
        list(label = comb_rel_ae_labels[i], val = toupper("Related"), NULL)
      )
    ) |>
    analyze(
      comb_rel_sae_vars[i],
      afun = a_freq_j,
      show_labels = "hidden",
      extra_args = append(
        extra_args_rr,
        list(label = comb_rel_sae_labels[i], val = "Y", NULL)
      )
    )
}

lyt <- lyt |>
  analyze(
    "TRDISCFL",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = append(
      extra_args_rr,
      list(label = "AE leading to permanent discontinuation of study treatment", val = "Y", NULL)
    )
  )

for (i in seq_along(comb_rel_aedth_vars)) {
  lyt <- lyt |>
    analyze(
      comb_rel_aedth_vars[i],
      afun = a_freq_j,
      show_labels = "hidden",
      extra_args = append(
        extra_args_rr,
        list(label = comb_rel_aedth_labels[i], val = "Y", NULL)
      )
    )
}

lyt <- lyt |>
  split_rows_by(
    "maxtox",
    split_fun = aesevall_spf,
    section_div = " "
  ) |>
  analyze(
    "maxtox",
    show_labels = "hidden",
    afun = a_freq_j,
    extra_args = append(extra_args_rr, NULL)
  ) |>
  analyze(
    "maxtoxcm",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = append(
      extra_args_rr,
      list(label = ">= Grade 3", val = "Y", NULL)
    )
  )

for (i in seq_along(comb_acnvars)) {
  lyt <- lyt |>
    split_rows_by(comb_acnvars[i], split_fun = dosemod_spf, section_div = " ") |>
    summarize_row_groups(
      comb_acnvars[i],
      cfun = a_freq_j,
      extra_args = list(
        label = comb_acn_labels[i],
        method = rr_method,
        ref_path = ref_path,
        .stats = c("count_unique_fraction")
      )
    ) |>
    analyze(
      comb_acnvars[i],
      table_names = comb_acnvars[i],
      a_freq_j,
      show_labels = "hidden",
      extra_args = append(
        extra_args_rr,
        list(
          excl_levels = excl_aeacn_levels,
          # label_map = dosemod_lblmap,
          drop_levels = TRUE
        )
      )
    )
}

lyt <- lyt |>
  split_rows_by(
    "AESER",
    split_fun = aeserall_spf,
    section_div = " "
  ) |>
  analyze(
    "AESDTH",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = append(
      extra_args_rr,
      list(label = "Death", val = "Y", NULL)
    )
  ) |>
  analyze(
    "AESLIFE",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = append(
      extra_args_rr,
      list(label = "Life-threatening", val = "Y", NULL)
    )
  ) |>
  analyze(
    "AESHOSP",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = append(
      extra_args_rr,
      list(label = "Requires or prolongs hospitalization", val = "Y", NULL)
    )
  ) |>
  analyze(
    "AESDISAB",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = append(
      extra_args_rr,
      list(
        label = "Persistent or significant disability/incapacity",
        val = "Y",
        NULL
      )
    )
  ) |>
  analyze(
    "AESCONG",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = append(
      extra_args_rr,
      list(label = "Congenital anomaly or birth defect", val = "Y", NULL)
    )
  ) |>
  analyze(
    "AESMIE",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = append(extra_args_rr, list(label = "Other medically important event", val = "Y", NULL))
  )


result <- build_table(lyt, adae, alt_counts_df = adsl, round_type = "sas")

################################################################################
# Post-Processing:
# - Remove N's from Risk cols
# - Prune any categories with all zeros.
################################################################################

result <- remove_col_count(result)
result <- suppressWarnings(safe_prune_table(
  result,
  prune_func = count_pruner(
    cat_exclude = c(
      "AEs",
      "SAEs",
      comb_rel_ae_labels,
      comb_rel_sae_labels,
      "AE leading to permanent discontinuation of any study treatment",
      comb_rel_aedth_labels,
      "Grade 1",
      "Grade 2",
      "Grade 3",
      "Grade 4",
      "Grade 5",
      ">= Grade 3",
      comb_acn_labels
    )
  )
))

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table:
################################################################################


colwidth <- c(64, 21, 21, 21, 21, 30, 30)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "landscape")
