library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSIMH01"
fileid <- write_path(opath, tblid)
titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")
popfls <- c("FASFL", "SAFFL")
popfl <- popfls[1]
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"

table_vars <- c("MHOCCUR", "MHCAT", "MHBODSYS", "MHDECOD", "MHTERM")

# Add flexibility to choose display level: "SOC", "PT", or "BOTH"
display_level <- "BOTH" # Options: "SOC", "PT", "BOTH"

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
  select(STUDYID, USUBJID, all_of(popfls), all_of(trtvar)) |>
  create_colspan_var(
    non_active_grp = "Placebo",
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = trtvar
  )

mh <- mh_jnj |>
  select(all_of(c("USUBJID", table_vars))) |>
  #user can filter appropriate value of MHCAT for medical histrory of intreset.
  filter(MHOCCUR == "Y" & stringr::str_to_upper(MHCAT) == "GENERAL MEDICAL HISTORY")

mh[table_vars] <- lapply(mh[table_vars], stringr::str_to_sentence)

mh <- mh |>
  mutate(
    MHDECOD = factor(case_when(
      MHDECOD == "" | is.na(MHDECOD) ~ paste0("Uncoded: ", MHTERM),
      .default = MHDECOD
    )),
    MHBODSYS = factor(case_when(
      MHBODSYS == "" | is.na(MHBODSYS) ~ "Uncoded",
      .default = MHBODSYS
    ))
  )

mh <- mh |> inner_join(adsl, by = c("USUBJID"))

## update label genmh_label to match selection that has been made
genmh_label <- "Subjects with >=1 medical history"

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

extra_args1 <- list(
  .stats = "count_unique_fraction",
  denom = "n_altdf"
)

# Build base layout
lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx",
  top_level_section_div = " "
) |>
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) |>
  split_cols_by(trtvar) |>
  add_overall_col("Total") |>
  analyze(
    "MHOCCUR",
    afun = a_freq_j,
    extra_args = list(
      label = genmh_label,
      .stats = c("count_unique_fraction")
    ),
    show_labels = "hidden",
    section_div = ""
  )

# Add SOC and/or PT based on display_level parameter
if (display_level == "BOTH") {
  lyt <- lyt |>
    split_rows_by(
      "MHBODSYS",
      split_label = "System Organ Class",
      split_fun = trim_levels_in_group("MHDECOD"),
      label_pos = "topleft",
      section_div = c(" ")
    ) |>
    summarize_row_groups(
      "MHBODSYS",
      cfun = a_freq_j,
      extra_args = extra_args1
    ) |>
    analyze(
      "MHDECOD",
      afun = a_freq_j,
      extra_args = extra_args1,
      indent_mod = 0L,
      show_labels = "hidden",
      nested = TRUE
    ) |>
    append_topleft("  Preferred Term, n (%)")
} else if (display_level == "SOC") {
  lyt <- lyt |>
    analyze(
      "MHBODSYS",
      afun = a_freq_j,
      extra_args = extra_args1,
      show_labels = "hidden",
    ) |>
    append_topleft("  System Organ Class, n (%)")
} else if (display_level == "PT") {
  lyt <- lyt |>
    analyze(
      "MHDECOD",
      afun = a_freq_j,
      extra_args = extra_args1,
      show_labels = "hidden",
    ) |>
    append_topleft("  Preferred Term, n (%)")
}

result <- build_table(lyt, mh, alt_counts_df = adsl, round_type = "sas")

################################################################################
# Post-Processing:
# - sort by descending count on risk diff column if it exists
# or active treatment columns if it does not
################################################################################

# Apply sorting based on display_level
if (display_level == "BOTH") {
  result <- sort_at_path(
    result,
    c("root", "MHBODSYS"),
    scorefun = jj_complex_scorefun("Total")
  )
  result <- sort_at_path(
    result,
    c("root", "MHBODSYS", "*", "MHDECOD"),
    scorefun = jj_complex_scorefun("Total")
  )
} else if (display_level == "SOC") {
  result <- sort_at_path(
    result,
    c("root", "MHBODSYS"),
    scorefun = jj_complex_scorefun("Total")
  )
} else if (display_level == "PT") {
  result <- sort_at_path(
    result,
    c("root", "MHDECOD"),
    scorefun = jj_complex_scorefun("Total")
  )
}


################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, titles)

################################################################################
# Convert to tbl file and output table:
################################################################################


colwidth <- c(64, 21, 21, 21, 23)

tt_to_tlgrtf(colwidths = colwidth, result, fileid)
