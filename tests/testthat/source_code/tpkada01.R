library(envsetup)
library(dplyr)
library(tidyr)
library(forcats)
library(rtables)
library(tern)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TPKADA01"
fileid <- write_path(opath, tblid)
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)
popfl <- "PKFL"
trtvar <- "TRT01A"
paramcd <- "XAN"
treatment_label <- "Antibodies to Active Study Agent"

# flag to indicate if ATPT is present in the study
# if TRUE, time point is concatenation of AVISIT and ATPT
# if FALSE, time point is AVISIT only
use_atpt <- TRUE

# Flags indicating whether to include certain statistics in the table
add_cv <- TRUE
add_interquartile_range <- TRUE

################################################################################
# Process data:
################################################################################

adsl <- adsl_jnj |>
  filter(.data[[popfl]] == "Y") |>
  select(STUDYID, USUBJID, all_of(c(trtvar, popfl))) |>
  filter(.data[[trtvar]] != "Placebo") |>
  mutate({{ trtvar }} := fct_drop(.data[[trtvar]])) |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c(
        "Xanomeline Low Dose",
        "Xanomeline High Dose"
      )
    )
  )


adpc <- adpc_jnj |>
  filter(PARAMCD == paramcd) |> # If not required this filter can be removed
  select(STUDYID, USUBJID, AVISIT, AVISITN, any_of(c("ATPT", "ATPTN")), AVAL)

ada_status_levels <- c(
  paste0("Positive for Treatment-emergent", treatment_label, "~[super b]"),
  paste0("Negative for Treatment-emergent", treatment_label, "~[super c]")
)

adishum <- adishum_jnj |>
  filter(PARAMCD %in% c("ADATRE", "ADANTRE") & IMEVFL == "Y") |>
  select(STUDYID, USUBJID, AVALC, IMEVFL, PARAMCD) |>
  tidyr::pivot_wider(
    id_cols = c(STUDYID, USUBJID, IMEVFL),
    names_from = PARAMCD,
    values_from = AVALC
  ) |>
  mutate(
    ADA_STATUS = factor(
      dplyr::case_when(
        ADATRE == "Y" ~ paste0("Positive for Treatment-emergent", treatment_label, "~[super b]"),
        ADANTRE == "Y" ~ paste0("Negative for Treatment-emergent", treatment_label, "~[super c]"),
        TRUE ~ NA_character_
      ),
      levels = ada_status_levels
    )
  ) |>
  filter(!is.na(ADA_STATUS))

adpc_final <- adpc |>
  inner_join(adishum, by = c("STUDYID", "USUBJID")) |>
  inner_join(adsl, by = c("STUDYID", "USUBJID"))


# derive selvisit from data, ordered by AVISITN then ATPTN
selvisit <- if (use_atpt) {
  adpc_final |>
    arrange(AVISITN, ATPTN) |>
    distinct(AVISIT, ATPT) |>
    mutate(AVISIT_ATPT = paste(AVISIT, ATPT, sep = " - ")) |>
    pull(AVISIT_ATPT)
} else {
  adpc_final |>
    arrange(AVISITN) |>
    distinct(AVISIT) |>
    pull(AVISIT)
}

adpc_final <- adpc_final |>
  mutate(
    AVISIT_ATPT = factor(
      if (use_atpt) paste(AVISIT, ATPT, sep = " - ") else as.character(AVISIT),
      levels = selvisit
    )
  )


# trick for alt_counts_df to work with col splitting
# add ADA_STATUS to adsl, assign all to extra level "N" (column will be used for N counts)
adslx <- adsl |>
  mutate(
    ADA_STATUS = factor("N", levels = c("N", ada_status_levels))
  )

################################################################################
# Define layout and build table:
################################################################################

lyt <- basic_table() |>
  split_cols_by(
    var = trtvar,
    show_colcounts = TRUE,
    colcount_format = "N=xx"
  ) |>
  split_cols_by(
    var = "ADA_STATUS",
    split_fun = drop_split_levels
  ) |>
  analyze(
    vars = "IMEVFL",
    show_labels = "hidden",
    section_div = " ",
    afun = a_freq_j,
    extra_args = list(
      val = "Y",
      label = "Subjects with evaluable samples~[super a]",
      .stats = "count_unique"
    )
  ) |>
  split_rows_by(
    var = "AVISIT_ATPT",
    split_label = "Time Point",
    label_pos = "topleft",
    section_div = " ",
    indent_mod = 1L
  ) |>
  analyze(
    vars = "AVAL",
    afun = a_summary,
    extra_args = list(
      .stats = c(
        "n",
        "mean_sd",
        "median",
        "range",
        if (add_cv) "cv" else NULL,
        if (add_interquartile_range) "quantiles" else NULL
      ),
      .labels = c(
        n = "N",
        mean_sd = "Mean (SD)",
        median = "Median",
        range = "Min, max",
        cv = "CV (%)",
        quantiles = "Interquartile range"
      ),
      .formats = c(
        n = jjcsformat_xx("xx"),
        mean_sd = format_sigfig_j(3, format = "xx (xx)"),
        median = format_sigfig_j(3, format = "xx"),
        range = format_sigfig_j(3, format = "(xx, xx)"),
        cv = jjcsformat_xx("xx.x"),
        quantiles = format_sigfig_j(3, format = "(xx, xx)")
      ),
      control = control_analyze_vars(
        quantiles = c(0.25, 0.75),
        quantile_type = 2
      ),
      .indent_mods = c(
        n = 0,
        mean_sd = 1,
        median = 1,
        range = 1,
        cv = 1,
        quantiles = 1
      )
    )
  )


result <- build_table(lyt, df = adpc_final, alt_counts_df = adslx, round_type = "sas")

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table:
################################################################################


colwidth <- c(64, 29, 31, 29, 29)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "landscape")
