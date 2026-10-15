library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSIDS02"
fileid <- write_path(opath, tblid)
popfl <- "FASFL"
trtvar <- "TRT01P"
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


################################################################################
# Process data:
################################################################################

adsl <- adsl_jnj |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c(
        "Xanomeline Low Dose",
        "Xanomeline High Dose",
        "Placebo"
      )
    )
  )

no_data_to_report <- function(df, var) {
  if (sum(is.na(df[[var]])) == length(df[[var]])) {
    df[[var]] <- factor(NA_character_, levels = "No data to report")
  }
  return(df)
}

adsl <- no_data_to_report(df = adsl, var = "DCTREAS")
adsl <- no_data_to_report(df = adsl, var = "DCSREAS")

adsl <- adsl |>
  filter(!!rlang::sym(popfl) == "Y") |>
  select(
    USUBJID,
    !!rlang::sym(trtvar),
    !!rlang::sym(popfl),
    SAFFL,
    PPROTFL,
    EOTSTT,
    DCTREAS,
    EOSSTT,
    DCSREAS,
    RACE
  ) |>
  create_colspan_var(
    non_active_grp = "Placebo",
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = trtvar
  )


################################################################################
# Define layout and build table:
################################################################################

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = "Placebo",
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

totdf <- tribble(
  ~valname                                                    ,
  ~label                                                      ,
  ~levelcombo                                                 ,
  ~exargs                                                     ,
  "Total"                                                     ,
  "Total"                                                     ,
  c("Xanomeline High Dose", "Xanomeline Low Dose", "Placebo") ,
  list()
)

rr_method <- "wald"
ref_path <- c("colspan_trt", " ", trtvar, "Placebo")
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
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) |>
  split_cols_by(trtvar) |>
  split_cols_by(
    trtvar,
    split_fun = add_combo_levels(totdf, keep_levels = "Total"),
    nested = FALSE
  ) |>
  # Subjects ongoing treatment
  analyze(
    "EOTSTT",
    show_labels = "hidden",
    afun = a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(
        label = "Subjects ongoing treatment",
        val = "ONGOING",
        riskdiff = FALSE,
        NULL
      )
    ),
    na_str = " "
  ) |>
  # Treatment disposition
  analyze(
    "EOTSTT",
    table_names = "Compl_Trt",
    show_labels = "hidden",
    afun = a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(label = "Completed treatment", val = "COMPLETED", NULL)
    )
  ) |>
  analyze(
    "EOTSTT",
    table_names = "DC_Trt",
    show_labels = "hidden",
    afun = a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(label = "Discontinued treatment", val = "DISCONTINUED", NULL)
    )
  ) |>
  analyze(
    "DCTREAS",
    show_labels = "hidden",
    indent_mod = 1,
    afun = a_freq_j,
    na_str = " ",
    extra_args = append(extra_args_rr, list(extrablankline = TRUE))
  ) |>
  # Subjects ongoing study
  analyze(
    "EOSSTT",
    show_labels = "hidden",
    afun = a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(
        label = "Subjects ongoing study",
        val = "ONGOING",
        riskdiff = FALSE,
        NULL
      )
    ),
    na_str = " "
  ) |>
  # Study disposition
  analyze(
    "EOSSTT",
    table_names = "Compl_Study",
    show_labels = "hidden",
    afun = a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(label = "Completed study", val = "COMPLETED", NULL)
    )
  ) |>
  analyze(
    "EOSSTT",
    show_labels = "hidden",
    table_names = "DC_Study",
    afun = a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(label = "Discontinued study", val = "DISCONTINUED", NULL)
    )
  ) |>
  analyze(
    "DCSREAS",
    show_labels = "hidden",
    indent_mod = 1,
    afun = a_freq_j,
    extra_args = append(extra_args_rr, NULL)
  )

result <- build_table(lyt, adsl, round_type = "sas")

################################################################################
# Post-Processing
################################################################################

result <- result |>
  sort_at_path(
    path = c(
      "ma_EOTSTT_Compl_Trt_DC_Trt_DCTREAS_EOSSTT_Compl_Study_DC_Study_DCSREAS",
      "DCTREAS"
    ),
    scorefun = jj_complex_scorefun(colpath = "Total", lastcat = "Other")
  ) |>
  sort_at_path(
    path = c(
      "ma_EOTSTT_Compl_Trt_DC_Trt_DCTREAS_EOSSTT_Compl_Study_DC_Study_DCSREAS",
      "DCSREAS"
    ),
    scorefun = jj_complex_scorefun(colpath = "Total", lastcat = "count_unique_fraction.Other")
  )

# Prune data driven output.
result <- result |>
  safe_prune_table(prune_func = keep_rows(keep_non_null_rows)) |>
  safe_prune_table(
    prune_func = count_pruner(
      cols = c("colspan_trt"),
      cat_exclude = c(
        "Completed study",
        "Completed treatment",
        "Discontinued study",
        "Discontinued treatment"
      )
    )
  )


################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)
################################################################################
# Convert to tbl file and output table:
################################################################################


colwidth <- c(44, 21, 21, 21, 23)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "landscape")
