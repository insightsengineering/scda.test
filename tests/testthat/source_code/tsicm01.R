library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)
library(tidyr)
library(stringr)

################################################################################
# Define script level parameters:
################################################################################

################################################################################
# - Define output ID and file location
# - Define treatment variable used (default=TRT01A)
# - Define population flag used (default=SAFFL)
# - Define dynamic ATC Variable
# - Define dynamic ATC Level
# - Choose whether or not you want to present a combined active treatment column (default=TRUE)
# - Define how to create combined treatment columns (if required)
################################################################################

tblid <- "TSICM01"
fileid <- write_path(opath, tblid)
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

trtvar <- "TRT01A"
popfl <- "SAFFL"

smnvar <- "CMDECOD"
atcvar <- c("CMLVL1", "CMLVL2", "CMLVL3", "CMLVL4")
atclevel <- c("1", "2", "3", "4")

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

# ################################################################################
# # Process Data:
# ################################################################################

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
  filter(ONTRTFL == "Y") |>
  select(USUBJID, CMTRT, ONTRTFL, all_of(atcvar), all_of(smnvar)) |>
  mutate(
    across(
      all_of(atcvar),
      ~ case_when(
        .x == "" ~ "Uncoded",
        .default = .x
      )
    ),
    across(
      all_of(atcvar),
      ~ str_to_sentence(.x)
    ),
    !!rlang::sym(smnvar) := case_when(
      .data[[smnvar]] == "" ~ paste0("Uncoded: ", CMTRT),
      .default = .data[[smnvar]]
    ),
    across(
      all_of(smnvar),
      ~ str_to_sentence(.x)
    )
  )
adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == "Placebo", " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

# join data together
cm <- adcm |> inner_join(adsl, by = c("USUBJID"))


if (length(adcm$ONTRTFL) == 0) {
  cm <- adcm |> right_join(adsl, by = c("USUBJID"))
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

extra_args_1 <- list(.stats = "count_unique_fraction")

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
  add_overall_col("Total") |>
  analyze(
    "ONTRTFL",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = append(
      extra_args_1,
      list(label = "Subjects with >=1 concomitant medication")
    )
  )

for (i in seq_along(atcvar)) {
  atcvars <- atcvar[[i]]

  lyt <- lyt |>
    split_rows_by(
      atcvars,
      child_labels = "hidden",
      split_label = paste0("ATC Level ", i),
      label_pos = "topleft",
      split_fun = trim_levels_in_group(smnvar),
      section_div = c(" "),
      indent_mod = 0L
    ) |>
    summarize_row_groups(atcvars, cfun = a_freq_j, extra_args = extra_args_1)
}

lyt <- lyt |>
  analyze(smnvar, afun = a_freq_j, extra_args = (extra_args_1)) |>
  append_topleft(paste(strrep("  ", length(atcvar)), "Standardized Medication Name, n (%)"))


result <- build_table(lyt, cm, alt_counts_df = adsl, round_type = "sas")

# If there is no data remove top row and display "No data to display" text
if (length(adcm$ONTRTFL) == 0) {
  result <- safe_prune_table(
    result,
    prune_func = remove_rows(
      removerowtext = "Subjects with >=1 concomitant medication"
    )
  )
}

#########################################################################################
# Post-Processing step to sort by descending count on total column:
#########################################################################################

if (length(adcm$ONTRTFL) != 0) {
  path_i <- c("root", atcvar[1])

  for (i in seq_along(atcvar)) {
    if (i == 1) {
      path_i <- c("root", atcvar[i])
    } else {
      path_i <- c(path_i, "*", atcvar[i])
    }

    result <- sort_at_path(
      result,
      path_i,
      scorefun = jj_complex_scorefun(colpath = "Total")
    )
  }
  result <- sort_at_path(
    result,
    c(path_i, "*", smnvar), ,
    scorefun = jj_complex_scorefun(colpath = "Total")
  )
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


colwidth <- c(64, 21, 21, 21, 21, 23)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, border_mat = bordmat, orientation = "landscape")
