library(envsetup)
library(dplyr)
library(forcats)
library(rtables)
library(tern)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TPK01b"
fileid <- write_path(opath, tblid)
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)
popfl <- "PKFL"
trtvar <- "TRT01A"
add_geometric_mean <- TRUE
add_cv <- TRUE
paramcd <- "XAN"


# flag to indicate if ATPT is present in the study
# if TRUE, time point is concatenation of AVISIT and ATPT
# if FALSE, time point is AVISIT only
use_atpt <- TRUE


trt_grps <- c("Xanomeline Low Dose", "Xanomeline High Dose")

################################################################################
# Process data:
################################################################################

adsl <- adsl_jnj |>
  filter(.data[[popfl]] == "Y") |>
  filter(.data[[trtvar]] == trt_grps) |>
  select(STUDYID, USUBJID, all_of(trtvar), PKFL) |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = trt_grps
    )
  )

adpc <- adpc_jnj |>
  filter(PARAMCD == paramcd) |> # If not required this filter can be removed
  select(STUDYID, USUBJID, AVISIT, AVISITN, any_of(c("ATPT", "ATPTN")), AVAL) |>
  inner_join(adsl, by = c("USUBJID", "STUDYID"))

# derive selvisit from data, ordered by AVISITN then ATPTN
selvisit <- if (use_atpt) {
  adpc |>
    arrange(AVISITN, ATPTN) |>
    distinct(AVISIT, ATPT) |>
    mutate(AVISIT_ATPT = paste(AVISIT, ATPT, sep = ", ")) |>
    pull(AVISIT_ATPT)
} else {
  adpc |>
    arrange(AVISITN) |>
    distinct(AVISIT) |>
    pull(AVISIT)
}

adpc <- adpc |>
  mutate(
    AVISIT_ATPT = factor(
      if (use_atpt) paste(AVISIT, ATPT, sep = ", ") else as.character(AVISIT),
      levels = selvisit
    )
  )

################################################################################
# Define layout and build table:
################################################################################

lyt <- basic_table() |>
  split_cols_by(
    var = trtvar,
    show_colcounts = TRUE,
    colcount_format = "N=xx",
    split_fun = drop_split_levels
  ) |>
  split_rows_by(
    var = "AVISIT_ATPT",
    split_label = "Time Point",
    label_pos = "topleft",
    child_labels = "hidden"
  ) |>
  analyze_vars_in_cols(
    vars = "AVAL",
    .stats = c(
      "n",
      "mean",
      "sd",
      "median",
      "min",
      "max",
      if (add_cv) "cv" else NULL,
      if (add_geometric_mean) "geom_mean" else NULL
    ),
    .labels = c(
      n = "N",
      mean = "Mean",
      sd = "SD",
      median = "Med",
      min = "Min",
      max = "Max",
      cv = "% CV",
      geom_mean = "Geometric Mean"
    ),
    .formats = c(
      n = jjcsformat_xx("xx"),
      mean = format_sigfig_j(3, format = "xx"),
      sd = format_sigfig_j(3, format = "xx"),
      median = format_sigfig_j(3, format = "xx"),
      min = format_sigfig_j(3, format = "xx"),
      max = format_sigfig_j(3, format = "xx"),
      cv = jjcsformat_xx("xx.x"),
      geom_mean = format_sigfig_j(3, format = "xx")
    )
  )

result <- build_table(lyt, df = adpc, alt_counts_df = adsl, round_type = "sas")

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table:
################################################################################


colwidth <- c(42, 7, 16, 16, 16, 16, 16, 12, 16, 7, 16, 16, 16, 16, 16, 12, 16)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "landscape", combined_rtf = TRUE, nosplitin = list(cols = trtvar))
