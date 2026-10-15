library(envsetup)
library(tern)
library(dplyr)
library(tidyr)
library(rtables)
library(junco)
library(forcats)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TIRNAB01"
fileid <- write_path(opath, tblid)
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

popfl <- "IMFL"
trtvar <- "TRT01A"
combined_colspan_trt <- TRUE
parqual <- "XANOMELINE"
# addon_filter <- expr(.data[[trtvar]] != "Placebo") # To be used with ADISHUM. If no filter needed
addon_filter <- expr(TRUE)
agt_antiagt_lbl <- "antibodies to active study agent"

################################################################################
# Process data:
################################################################################

adsl <- adsl_jnj |>
  filter(.data[[popfl]] == "Y", .data[[trtvar]] != "Placebo") |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c(
        "Xanomeline Low Dose",
        "Xanomeline High Dose"
      )
    ),
    {{ trtvar }} := fct_drop(.data[[trtvar]])
  ) |>
  mutate(colspan_trt = "Active Study Agent") |>
  select(USUBJID, all_of(trtvar), all_of(popfl), colspan_trt)

adishum <- adishum_jnj |>
  filter(.data[[popfl]] == "Y", PARQUAL == parqual, !!addon_filter) |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c(
        "Xanomeline Low Dose",
        "Xanomeline High Dose"
      )
    ),
    {{ trtvar }} := fct_drop(.data[[trtvar]])
  ) |>
  select(USUBJID, all_of(trtvar), all_of(popfl), PARAMCD, AVALC)

adishum_pivot <- adishum |>
  filter(PARAMCD %in% c("ADATRE", "NABPOS", "NABNEG")) |>
  pivot_wider(
    id_cols = USUBJID,
    names_from = PARAMCD,
    values_from = AVALC
  ) |>
  mutate(NAB = ifelse(ADATRE == "Y" & (NABPOS == "Y" | NABNEG == "Y"), "Y", NA))

adishum <- adishum_pivot |>
  right_join(adsl, by = "USUBJID")

# programming note 3: check if any subjects positive per section

has_adatre <- any(adishum$ADATRE == "Y", na.rm = TRUE)

################################################################################
# Create Layout:
################################################################################

lyt <- basic_table() |>
  split_cols_by(
    "colspan_trt",
    split_fun = drop_split_levels
  ) |>
  append_topleft("NAb Status")

if (combined_colspan_trt) {
  lyt <- lyt |>
    split_cols_by(
      var = trtvar,
      show_colcounts = TRUE,
      colcount_format = "N=xx",
      split_fun = add_overall_level("Combined", first = FALSE)
    )
} else {
  lyt <- lyt |>
    split_cols_by(
      var = trtvar,
      show_colcounts = TRUE,
      colcount_format = "N=xx"
    )
}

lyt <- lyt |>
  analyze(
    "ADATRE",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      label = paste("Subjects positive for treatment-emergent", agt_antiagt_lbl, "at any time~[super a]"),
      val = "Y",
      .stats = c("count_unique")
    )
  ) |>
  split_rows_by("NAB", split_fun = remove_split_levels(c("N")), child_labels = "hidden") |>
  analyze(
    "NAB",
    indent_mod = 1L,
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      label = "Subjects evaluable for neutralizing antibodies~[super b]",
      val = "Y",
      .stats = c("count_unique")
    )
  ) |>
  analyze(
    "NABPOS",
    indent_mod = 2L,
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      label = "Subjects positive for neutralizing antibodies~[super c], n (%)",
      val = "Y",
      .stats = c("count_unique_fraction"),
      denom = "n_parentdf",
      denom_by = "NAB"
    )
  ) |>
  analyze(
    "NABNEG",
    indent_mod = 2L,
    afun = a_freq_j,
    show_labels = "hidden",
    section_div = " ",
    extra_args = list(
      label = "Subjects negative for neutralizing antibodies~[super d], n (%)",
      val = "Y",
      .stats = c("count_unique_fraction"),
      denom = "n_parentdf",
      denom_by = "NAB"
    )
  ) |>
  analyze(
    "NABPOS",
    table_names = "Overall_positive",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      label = "Overall incidence of subjects positive for neutralizing antibodies~[super e], n (%)",
      val = "Y",
      .stats = c("count_unique_fraction")
    )
  ) |>
  analyze(
    "NABNEG",
    table_names = "Overall_negative",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      label = "Overall incidence of subjects negative for neutralizing antibodies~[super e], n (%)",
      val = "Y",
      .stats = c("count_unique_fraction")
    )
  )

################################################################################
# Create Table Shell:
################################################################################

result <- build_table(lyt, adishum, alt_counts_df = adsl, round_type = "sas")

################################################################################
# Post-Processing:
################################################################################

result <- safe_prune_table(result, prune_func = prune_empty_level)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table:
################################################################################


colwidth <- c(64, 21, 21, 21)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "landscape")
