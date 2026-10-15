library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)

###############################################################################
# Define script level parameters
###############################################################################

tblid <- "TSIDS01"
fileid <- write_path(opath, tblid)
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)
ran_enrl_var <- "RANDFL"
if (ran_enrl_var == "RANDFL") {
  ran_enrl_lbl <- "randomized"
} else if (ran_enrl_var == "ENRLFL") {
  ran_enrl_lbl <- "enrolled"
}
###############################################################################
# Process data
###############################################################################

adsl <- adsl_jnj |>
  filter(SCRNFL == "Y") |>
  select(
    STUDYID,
    USUBJID,
    SCRNFL,
    SCRFFL,
    DCSCREEN,
    RESCRNFL,
    all_of(ran_enrl_var),
    TRT01P
  )

###############################################################################
# Define layout and build table
###############################################################################

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx",
  top_level_section_div = " "
) |>
  add_overall_col(label = "Total") |>
  split_rows_by("SCRFFL", split_fun = keep_split_levels("Y")) |>
  summarize_row_groups(
    "SCRFFL",
    cfun = a_freq_j,
    extra_args = list(
      riskdiff = FALSE,
      .stats = c("count_unique_fraction"),
      label = "Screening failures"
    )
  ) |>
  analyze(
    "DCSCREEN",
    afun = a_freq_j,
    extra_args = list(
      riskdiff = FALSE,
      .stats = c("count_unique_fraction")
    )
  ) |>
  split_rows_by("RESCRNFL", split_fun = keep_split_levels("Y")) |>
  summarize_row_groups(
    "RESCRNFL",
    cfun = a_freq_j,
    extra_args = list(
      riskdiff = FALSE,
      .stats = c("count_unique"),
      label = "Subjects re-screened"
    )
  ) |>
  analyze(
    "SCRFFL",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      riskdiff = FALSE,
      .stats = c("count_unique_fraction"),
      val = "Y",
      label = "Screening failures",
      denom = "n_rowdf"
    )
  ) |>
  analyze(
    ran_enrl_var,
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      riskdiff = FALSE,
      .stats = c("count_unique_fraction"),
      val = "Y",
      label = paste0("Subjects ", ran_enrl_lbl),
      denom = "n_rowdf"
    )
  ) |>
  split_rows_by(ran_enrl_var, split_fun = keep_split_levels("Y")) |>
  summarize_row_groups(
    ran_enrl_var,
    cfun = a_freq_j,
    extra_args = list(
      riskdiff = FALSE,
      .stats = c("count_unique_fraction"),
      label = paste0("Subjects ", ran_enrl_lbl)
    )
  )
result <- build_table(lyt, df = adsl, alt_counts_df = adsl, round_type = "sas")

###############################################################################
# Post-processing
###############################################################################

# Post-processing step to sort by descending count in the Combined column

result <- sort_at_path(
  tt = result,
  path = c("root", "SCRFFL", "Y", "DCSCREEN"),
  scorefun = jj_complex_scorefun(
    spanningheadercolvar = NULL,
    colpath = c("Total", "Total"),
    firstcat = NULL,
    lastcat = "count_unique_fraction.Other"
  )
)


if (nrow(adsl) == 0) {
  # Post-processing step to remove table rows with all 0 or NA values
  result <- safe_prune_table(result, prune_func = prune_empty_level)
} else {
  prune_empty_level_tablerow <- function(tt) {
    if (is(tt, "ContentRow")) {
      return(FALSE)
    }
    if (is(tt, "TableRow")) {
      return(FALSE)
    }
    kids <- tree_children(tt)
    length(kids) == 0
  }

  result <- result |> trim_rows(prune_empty_level_tablerow)
}

###############################################################################
# Retrieve titles and footnotes
###############################################################################

result <- set_titles(result, tab_titles)

###############################################################################
# Convert to tbl file and output table
###############################################################################


colwidth <- c(64, 23)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid)
