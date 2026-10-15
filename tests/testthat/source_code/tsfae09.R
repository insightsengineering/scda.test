library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSFAE09"
fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

eair_stats <- "n_eair" # current shell
# eair_stats <- "eair_n_py" # alternative shell

# for sorting function
# sorting should be performed on values of eair,
# which are second values in cellvalue
# set the appropriate cellvalue_index for usage in jj_complex_scorefun
cv_index <- 1
if (eair_stats == "n_eair") {
  cv_index <- 2
}


################################################################################
# Process data:
################################################################################

adexsum <- adexsum_jnj |>
  filter(!!rlang::sym(popfl) == "Y" & PARAMCD == "TRTDURY") |>
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
  mutate(
    rrisk_header = "Incidence Rate Difference (95% CI)",
    rrisk_label = paste(!!rlang::sym(trtvar), "vs", ctrl_grp),
    TRTDURY = AVAL
  ) |>
  select(
    USUBJID,
    !!rlang::sym(trtvar),
    colspan_trt,
    rrisk_header,
    rrisk_label,
    TRTDURY
  )

adae <- adae_jnj |>
  mutate(
    AEDECOD = case_when(
      AEDECOD == "" ~ paste0("Uncoded: ", AETERM),
      .default = AEDECOD
    )
  ) |>
  filter(TRTEMFL == "Y" & AOCCPFL == "Y") |>
  select(USUBJID, AEDECOD, ASTDY, AOCCPFL)

#  join -- -- subjects without ae will be handled via alt_counts_df dataframe
aefup <- right_join(adae, adexsum, by = "USUBJID")

colspan_trt_map <- create_colspan_map(
  adexsum,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)
ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)

################################################################################
# Define layout and build table:
################################################################################

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
  split_cols_by("rrisk_header", nested = FALSE) |>
  split_cols_by(
    trtvar,
    labels_var = "rrisk_label",
    split_fun = remove_split_levels(ctrl_grp)
  ) |>
  analyze(
    "TRTDURY",
    nested = FALSE,
    show_labels = "hidden",
    afun = a_patyrs_j,
    extra_args = list(.labels = c(patyrs = "Subject years~[super a]"))
  ) |>
  analyze(
    vars = "AEDECOD",
    nested = FALSE,
    afun = a_eair100_j,
    extra_args = list(
      fup_var = "TRTDURY",
      occ_var = "AOCCPFL",
      occ_dy = "ASTDY",
      ref_path = ref_path,
      .stats = eair_stats,
      drop_levels = TRUE,
      row_labels_adj = TRUE
    )
  ) |>
  append_topleft("Preferred Term, n (EAIR Per 100 SY)")


result <- build_table(lyt, aefup, alt_counts_df = adexsum)


################################################################################
# Post-Processing:
# - Remove Ns from Risk cols
# - Sort by descending AEDECOD in the combined Xanomeline High Dose column
################################################################################
result <- result |>
  sort_at_path(
    path = c("AEDECOD"),
    scorefun = jj_complex_scorefun(
      colpath = "Xanomeline High Dose",
      cellvalue_index = cv_index
    )
  )

result <- remove_col_count(result)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table:
################################################################################


colwidth <- c(52, 18, 18, 16, 39, 37)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "landscape")
