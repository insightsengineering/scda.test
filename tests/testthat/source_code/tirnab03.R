library(envsetup)
library(tern)
library(dplyr)
library(tidyr)
library(rtables)
library(junco)
library(forcats)
library(tidyselect)
library(purrr)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TIRNAB03"
fileid <- write_path(opath, tblid)
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

popfl <- "SAFFL"
trtvar <- "TRT01A"
combined_colspan_trt <- TRUE

################################################################################
# ADISHUM parameters/ filter criteria:
################################################################################
parqual <- "XANOMELINE"
addon_filter <- expr(.data[[trtvar]] != "Placebo") # To be used with ADISHUM.
# addon_filter <- expr(TRUE) # If no filter required
agent_name <- "active study agent"
agt_antiagt_lbl <- "antibodies to active study agent"
sev_grade_fl <- "AESEV"
# sev_grade_fl <- "AETOXGRN"
sev_grade_lbl <- "severe"
# sev_grade_lbl <- ">=grade 3"

ae_filter <- expr(SAFFL == "Y" & AESCAT == "Injection site reaction")
aedosdtvar <- "DOSEDT"

adexsum_filter <- expr(SAFFL == "Y")
adexsumprmcd <- "TNUMDOS"

################################################################################
# Process data:
################################################################################

# Pre-process ADSL

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

# Pre-process ADISHUM

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
  select(
    USUBJID,
    all_of(trtvar),
    all_of(popfl),
    PARAMCD,
    AVALC
  )

adishum_pivot <- adishum |>
  filter(PARAMCD %in% c("ADATRE", "NABPOS", "NABNEG")) |>
  pivot_wider(
    id_cols = c(USUBJID),
    names_from = PARAMCD,
    values_from = AVALC
  ) |>
  mutate(NAB = ifelse(NABPOS == "Y" | NABNEG == "Y", "Y", NA))

adishum <- adishum_pivot |>
  right_join(adsl, by = "USUBJID") |>
  distinct(USUBJID, .keep_all = TRUE)


# Pre-process ADAE

adae_base <- adae_jnj |>
  filter(!!ae_filter)

adae_nabstat <- adae_base |>
  filter(NABSTAT %in% c("POSITIVE", "NEGATIVE")) |>
  distinct(USUBJID, NABSTAT)

adae_sev <- adae_base |>
  filter(toupper(AESEV) == "SEVERE") |>
  mutate(SEVFL = "Y") |>
  distinct(USUBJID, SEVFL)

adae_tox <- adae_base |>
  group_by(USUBJID) |>
  summarise(
    TOXFL = if_else(any(AETOXGRN >= 3, na.rm = TRUE), "Y", NA_character_),
    .groups = "drop"
  ) |>
  filter(TOXFL == "Y")

adae_ser <- adae_base |>
  filter(AESER == "Y") |>
  distinct(USUBJID, AESER)

adae_dct <- adae_base |>
  filter(TRDISCFL == "Y") |>
  distinct(USUBJID, TRDISCFL)

adae_dosct <- adae_base |>
  group_by(USUBJID) |>
  summarise(
    NDOSDT = n_distinct(!!sym(aedosdtvar), na.rm = TRUE),
    .groups = "drop"
  )

# Pre-process ADEXSUM

adexsum_base <- adexsum_jnj |>
  filter(!!adexsum_filter, PARAMCD == adexsumprmcd) |>
  select(USUBJID, AVAL) |>
  rename(TINFUS = AVAL)

# Join all data together

join_df <- list(adae_nabstat, adae_sev, adae_tox, adae_ser, adae_dct, adae_dosct, adexsum_base)

adishum_f1 <- purrr::reduce(
  join_df,
  ~ left_join(.x, .y, by = "USUBJID"),
  .init = adishum
)

adishum_f <- adishum_f1 |>
  left_join(
    adae_base |>
      distinct(USUBJID) |>
      mutate(AEFL = "Y"),
    by = "USUBJID"
  )

sg_var <- if (sev_grade_fl == "AESEV") {
  "SEVFL"
} else if (sev_grade_fl == "AETOXGRN") {
  "TOXFL"
}

adishum_final <- adishum_f |>
  mutate(
    P1FL = ifelse(NABSTAT == "POSITIVE" & AEFL == "Y", "Y", NA),
    P2FL = ifelse(NABSTAT == "POSITIVE" & !!sym(sg_var) == "Y", "Y", NA),
    P3FL = ifelse(NABSTAT == "POSITIVE" & AESER == "Y", "Y", NA),
    P4FL = ifelse(NABSTAT == "POSITIVE" & TRDISCFL == "Y", "Y", NA),
    PNDOSDT = ifelse(NABSTAT == "POSITIVE", NDOSDT, NA),
    PTINFUS = ifelse(NABSTAT == "POSITIVE", TINFUS, NA),
    N1FL = ifelse(NABSTAT == "NEGATIVE" & AEFL == "Y", "Y", NA),
    N2FL = ifelse(NABSTAT == "NEGATIVE" & !!sym(sg_var) == "Y", "Y", NA),
    N3FL = ifelse(NABSTAT == "NEGATIVE" & AESER == "Y", "Y", NA),
    N4FL = ifelse(NABSTAT == "NEGATIVE" & TRDISCFL == "Y", "Y", NA),
    NNDOSDT = ifelse(NABSTAT == "NEGATIVE", NDOSDT, NA),
    NTINFUS = ifelse(NABSTAT == "NEGATIVE", TINFUS, NA)
  )


################################################################################
# Create Layout:
################################################################################

lyt <- basic_table() |>
  split_cols_by(
    "colspan_trt",
    split_fun = drop_split_levels
  )

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
  analyze(
    "NAB",
    indent_mod = 1L,
    afun = a_freq_j,
    show_labels = "hidden",
    section_div = " ",
    extra_args = list(
      label = "Subjects evaluable for neutralizing antibodies ~[super b]",
      val = "Y",
      .stats = c("count_unique")
    )
  ) |>
  split_rows_by("NABPOS", split_fun = keep_split_levels("Y"), child_labels = "hidden") |>
  analyze(
    "NABPOS",
    indent_mod = 2L,
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      label = paste("Subjects positive for neutralizing", agt_antiagt_lbl, "~[super c,e]"),
      val = "Y",
      .stats = c("count_unique")
    )
  ) |>
  analyze(
    "P1FL",
    indent_mod = 3L,
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      label = "Subjects with injection site reaction, n (%)",
      val = "Y",
      .stats = c("count_unique_fraction"),
      denom = "n_parentdf",
      denom_by = "NABPOS"
    )
  ) |>
  analyze(
    "P2FL",
    indent_mod = 3L,
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      label = paste("Subjects with", sev_grade_lbl, "injection site reaction, n (%)"),
      val = "Y",
      .stats = c("count_unique_fraction"),
      denom = "n_parentdf",
      denom_by = "NABPOS"
    )
  ) |>
  analyze(
    "P3FL",
    indent_mod = 3L,
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      label = "Subjects with serious injection site reaction, n (%)",
      val = "Y",
      .stats = c("count_unique_fraction"),
      denom = "n_parentdf",
      denom_by = "NABPOS"
    )
  ) |>
  analyze(
    "P4FL",
    indent_mod = 3L,
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      label = "Subjects with injection site reaction leading to treatment discontinuation, n (%)",
      val = "Y",
      .stats = c("count_unique_fraction"),
      denom = "n_parentdf",
      denom_by = "NABPOS"
    )
  ) |>
  analyze(
    "PTINFUS",
    afun = a_sum_ratio_j,
    show_labels = "hidden",
    indent_mod = 3L,
    extra_args = list(
      .stats = "sum_unique",
      .labels = c(sum_unique = paste("Total number of", agent_name, "injections")),
      id_var = "USUBJID"
    )
  ) |>
  analyze(
    "PNDOSDT",
    afun = a_sum_ratio_j,
    show_labels = "hidden",
    indent_mod = 4L,
    section_div = " ",
    extra_args = list(
      .stats = "ratio_unique",
      denom_by = "PTINFUS",
      .labels = c(ratio_unique = "Injections with injection site reactions~[super f], n (%)"),
      id_var = "USUBJID"
    )
  ) |>
  split_rows_by("NABNEG", nested = FALSE, split_fun = keep_split_levels("Y"), child_labels = "hidden") |>
  analyze(
    "NABNEG",
    indent_mod = 2L,
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      label = paste("Subjects negative for neutralizing", agt_antiagt_lbl, "~[super d,e]"),
      val = "Y",
      .stats = c("count_unique")
    )
  ) |>
  analyze(
    "N1FL",
    indent_mod = 3L,
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      label = "Subjects with injection site reaction, n (%)",
      val = "Y",
      .stats = c("count_unique_fraction"),
      denom = "n_parentdf",
      denom_by = "NABNEG"
    )
  ) |>
  analyze(
    "N2FL",
    indent_mod = 3L,
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      label = paste("Subjects with", sev_grade_lbl, "injection site reaction, n (%)"),
      val = "Y",
      .stats = c("count_unique_fraction"),
      denom = "n_parentdf",
      denom_by = "NABNEG"
    )
  ) |>
  analyze(
    "N3FL",
    indent_mod = 3L,
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      label = "Subjects with serious injection site reaction, n (%)",
      val = "Y",
      .stats = c("count_unique_fraction"),
      denom = "n_parentdf",
      denom_by = "NABNEG"
    )
  ) |>
  analyze(
    "N4FL",
    indent_mod = 3L,
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      label = "Subjects with injection site reaction leading to treatment discontinuation, n (%)",
      val = "Y",
      .stats = c("count_unique_fraction"),
      denom = "n_parentdf",
      denom_by = "NABNEG"
    )
  ) |>
  analyze(
    "NTINFUS",
    afun = a_sum_ratio_j,
    show_labels = "hidden",
    indent_mod = 3L,
    extra_args = list(
      .stats = "sum_unique",
      .labels = c(sum_unique = paste("Total number of", agent_name, "injections")),
      id_var = "USUBJID"
    )
  ) |>
  analyze(
    "NNDOSDT",
    afun = a_sum_ratio_j,
    show_labels = "hidden",
    indent_mod = 4L,
    extra_args = list(
      .stats = "ratio_unique",
      denom_by = "NTINFUS",
      .labels = c(ratio_unique = "Injections with injection site reactions~[super f], n (%)"),
      id_var = "USUBJID"
    )
  )

################################################################################
# Create Table Shell:
################################################################################

result <- build_table(lyt, adishum_final, alt_counts_df = adsl, round_type = "sas")

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table:
################################################################################


colwidth <- c(64, 21, 21, 21)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "landscape")
