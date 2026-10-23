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
# - Define substitution flag for concomitant medications (default=ONTRTFL)
# - Define ATC variables
# - Define dynamic coded term variable
# - Choose whether or not you want to present a combined active treatment column (default=TRUE)
# - Define how to create combined treatment columns (if required)
################################################################################

tblid <- "TSICM08"
fileid <- write_path(opath, tblid)
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

trtvar <- "TRT01A"
popfl <- "SAFFL"
substfl <- "ONTRTFL"

atcvars <- c("CMLVL1", "CMLVL2", "CMLVL3", "CMLVL4")
codedtermvar <- "CMBASPRF"

combined_colspan_trt <- TRUE

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

# Read in required data
adsl <- adsl_jnj |>
  filter(!!rlang::sym(popfl) == "Y") |>
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
  select(USUBJID, all_of(trtvar), all_of(popfl))

adcm <- adcm_jnj |>
  filter(!!rlang::sym(substfl) == "Y") |>
  select(
    USUBJID,
    all_of(substfl),
    all_of(codedtermvar),
    starts_with("CQ") & ends_with("NAM"),
    starts_with("CMLVL"),
    CMTRT
  ) |>
  mutate(
    across(
      all_of(atcvars),
      ~ case_when(
        .x == "" ~ "Uncoded",
        .default = .x
      )
    ),
    across(
      all_of(atcvars),
      ~ as.factor(stringr::str_to_sentence(.x))
    ),
    !!rlang::sym(codedtermvar) := case_when(
      .data[[codedtermvar]] == "" ~ paste0("Uncoded: ", CMTRT),
      .default = .data[[codedtermvar]]
    ),
    across(
      all_of(codedtermvar),
      ~ as.factor(stringr::str_to_sentence(.x))
    )
  )

# Work out how many CQzzNAM vars we have
cqzznamvars <- adcm |>
  select(starts_with("CQ") & ends_with("NAM"))

countcqzznamvars <- length(names(cqzznamvars))

# Create new variable that binds all special interest data together in vertical structure
mat <- matrix(ncol = 0, nrow = 0)
adcm_v <- data.frame(mat)

for (i in 1:countcqzznamvars) {
  if (i < 10) {
    ix <- paste0("0", i)
  } else {
    ix <- i
  }

  cqdata <- adcm |>
    mutate(cqnamvar = as.factor(!!as.name(paste0("CQ", ix, "NAM")))) |>
    filter(!is.na(cqnamvar))

  adcm_v <- bind_rows(adcm_v, cqdata)
}

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == "Placebo", " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

# join data together
cm <- adcm_v |> inner_join(adsl, by = c("USUBJID"))

if (length(adcm[[substfl]]) == 0) {
  cm <- adcm_v |> right_join(adsl, by = c("USUBJID"))
}

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = "Placebo",
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

################################################################################
# Define layout and build table:
################################################################################

extra_args_1 <- list(
  denom = "n_altdf",
  .stats = c("count_unique_fraction")
)

lyt <- rtables::basic_table(
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

lyt <- lyt |>
  add_overall_col("Total")

lyt <- lyt |>
  split_rows_by(
    "cqnamvar",
    child_labels = "hidden",
    split_label = "Interest Category",
    label_pos = "topleft",
    split_fun = trim_levels_in_group(codedtermvar),
    section_div = c(" "),
    indent_mod = 0L
  ) |>
  summarize_row_groups(
    "cqnamvar",
    cfun = a_freq_j,
    extra_args = extra_args_1
  )

for (i in seq_along(atcvars)) {
  atcvar <- atcvars[[i]]

  lyt <- lyt |>
    split_rows_by(
      atcvar,
      child_labels = "hidden",
      split_label = paste0("ATC Level ", sub(".*([0-9]{1,2})$", "\\1", atcvar)),
      label_pos = "topleft",
      split_fun = trim_levels_in_group(codedtermvar),
      section_div = c(" "),
      indent_mod = 0L
    ) |>
    summarize_row_groups(atcvar, cfun = a_freq_j, extra_args = extra_args_1)
}

lyt <- lyt |>
  analyze(codedtermvar, afun = a_freq_j, extra_args = (extra_args_1)) |>
  append_topleft(paste(strrep("  ", length(atcvars)), " Base Preferred Term, n (%)"))

result <- build_table(lyt, cm, alt_counts_df = adsl, round_type = "sas")

#########################################################################################
# Post-Processing step to sort by descending count on total column
#########################################################################################

if (length(adcm[[substfl]]) != 0) {
  result <- sort_at_path(
    result,
    c("cqnamvar"),
    scorefun = jj_complex_scorefun(colpath = "Total")
  )

  for (i in seq_along(atcvars)) {
    paths <- unlist(lapply(seq_along(atcvars), function(i) c(atcvars[[i]], "*")))
    result <- sort_at_path(
      result,
      c("cqnamvar", "*", paths, codedtermvar),
      scorefun = jj_complex_scorefun(colpath = "Total")
    )
  }
}

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
  # Create extra_row(s) based on the difference between top_left length and levels_cols
  num_extra_rows <- length(top_left(result)) - levels_cols
  for (i in 1:num_extra_rows) {
    extra_row <- rep(0, ncol(bordmat))
    bordmat <- rbind(extra_row, bordmat)
  }
  rownames(bordmat) <- NULL
}

################################################################################
# Convert to tbl file and output table
################################################################################

colwidth <- c(64, 19, 17, 21, 17, 19)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, border_mat = bordmat, label_width_ins = 2.4, orientation = "landscape")
