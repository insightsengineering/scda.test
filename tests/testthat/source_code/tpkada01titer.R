library(envsetup)
library(dplyr)
library(forcats)
library(rtables)
library(tern)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TPKADA01titer"
fileid <- write_path(opath, tblid)
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")
popfl <- "PKFL"
trtvar <- "TRT01A"
paramcd <- "XAN"
treatment_label <- "Antibodies to Active Study Agent"

# flag to indicate if ATPT is present in the study
# if TRUE, time point is concatenation of AVISIT and ATPT
# if FALSE, time point is AVISIT only
use_atpt <- TRUE

# Flags indicating whether to include certain statistics in the table
add_interquartile_range <- TRUE

# Titer category levels from ADISHUM AVALCAT1 (PARAMCD = ADATREPT)
# Order should match the mock shell: <10, 10 to <100, 100 to <1000, >=1000
titer_levels <- c("<10", "10 to <100", "100 to <1000", ">=1000")

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
  filter(is.null(paramcd) | PARAMCD == paramcd) |> # If not required this filter can be removed
  select(STUDYID, USUBJID, AVISIT, AVISITN, any_of(c("ATPT", "ATPTN")), AVAL)

ada_status_levels <- c(
  paste0("Negative for Treatment-emergent ", treatment_label, "~[super a]"),
  paste0("Positive for Treatment-emergent ", treatment_label, "~[super b]")
)

# ADA status columns: ADANTRE (Negative) and ADATRE (Positive)
adishum_ada <- adishum_jnj |>
  filter(PARAMCD %in% c("ADATRE", "ADANTRE") & IMEVFL == "Y") |>
  select(STUDYID, USUBJID, AVALC, IMEVFL, PARAMCD) |>
  tidyr::pivot_wider(
    id_cols = c(STUDYID, USUBJID, IMEVFL),
    names_from = PARAMCD,
    values_from = AVALC,
    values_fn = dplyr::first
  ) |>
  mutate(
    ADA_STATUS = factor(
      dplyr::case_when(
        ADATRE == "Y" ~ paste0("Positive for Treatment-emergent ", treatment_label, "~[super b]"),
        ADANTRE == "Y" ~ paste0("Negative for Treatment-emergent ", treatment_label, "~[super a]"),
        TRUE ~ NA_character_
      ),
      levels = ada_status_levels
    )
  ) |>
  filter(!is.na(ADA_STATUS))

# Titer columns: ADATREPT with AVALCAT1 categories — only Positive subjects
adishum_titer <- adishum_jnj |>
  filter(PARAMCD == "ADATREPT" & IMEVFL == "Y") |>
  select(STUDYID, USUBJID, AVALC, AVALCAT1, IMEVFL) |>
  mutate(
    TITER_CAT = factor(
      AVALCAT1,
      levels = titer_levels
    )
  ) |>
  filter(!is.na(TITER_CAT))

#Spanning header for titer columns
titer_header <- paste(
  "Peak Titers for Subjects Positive for Treatment-emergent",
  treatment_label
)

# Join adpc with ADA status (many-to-many: one row per ADA status per PK record)
adpc_ada <- adpc |>
  inner_join(adishum_ada, by = c("STUDYID", "USUBJID")) |>
  inner_join(adsl, by = c("STUDYID", "USUBJID")) |>
  mutate(
    ADA_STATUS = factor(as.character(ADA_STATUS), levels = ada_status_levels)
  )

# Join adpc with titer categories (only Positive subjects with titer data)
adpc_titer <- adpc |>
  inner_join(adishum_titer, by = c("STUDYID", "USUBJID")) |>
  inner_join(adsl, by = c("STUDYID", "USUBJID")) |>
  mutate(
    ADA_STATUS = factor(as.character(TITER_CAT), levels = titer_levels),
    TITER_HEADER = factor(titer_header, levels = titer_header)
  ) |>
  select(-TITER_CAT)

# derive selvisit from combined data, ordered by AVISITN then ATPTN
selvisit <- if (use_atpt) {
  bind_rows(adpc_ada, adpc_titer) |>
    arrange(AVISITN, ATPTN) |>
    distinct(AVISIT, ATPT) |>
    mutate(AVISIT_ATPT = paste(AVISIT, ATPT, sep = ", ")) |>
    pull(AVISIT_ATPT)
} else {
  bind_rows(adpc_ada, adpc_titer) |>
    arrange(AVISITN) |>
    distinct(AVISIT) |>
    pull(AVISIT)
}

adpc_ada <- adpc_ada |>
  mutate(
    AVISIT_ATPT = factor(
      if (use_atpt) paste(AVISIT, ATPT, sep = ", ") else as.character(AVISIT),
      levels = selvisit
    ),
    BLANK_HEADER = factor(" ", levels = " ")
  )

adpc_titer <- adpc_titer |>
  mutate(
    AVISIT_ATPT = factor(
      if (use_atpt) paste(AVISIT, ATPT, sep = ", ") else as.character(AVISIT),
      levels = selvisit
    )
  )

# alt_counts_df for ADA status table — one row per subject per ADA status level
adslx_ada <- adishum_ada |>
  select(STUDYID, USUBJID, ADA_STATUS) |>
  distinct() |>
  inner_join(adsl, by = c("STUDYID", "USUBJID")) |>
  mutate(BLANK_HEADER = factor(" ", levels = " "))

# alt_counts_df for titer table — one row per subject per titer level
adslx_titer <- adishum_titer |>
  select(STUDYID, USUBJID, TITER_CAT) |>
  distinct() |>
  inner_join(adsl, by = c("STUDYID", "USUBJID")) |>
  mutate(
    ADA_STATUS = factor(as.character(TITER_CAT), levels = titer_levels),
    TITER_HEADER = factor(titer_header, levels = titer_header)
  ) |>
  select(-TITER_CAT)


# Table 1: ADA status columns (Negative + Positive) — blank top-level span to match tbl_titer header depth
#adpc_ada <- adpc_ada |> mutate(BLANK_HEADER = factor(" ", levels = " "))
#adslx_ada <- adslx_ada |> mutate(BLANK_HEADER = factor(" ", levels = " "))

################################################################################
# Define layout and build table:
################################################################################

# Shared analyze layout helper (reused for both tables)
lyt_analyze <- function(lyt) {
  lyt |>
    analyze(
      vars = "IMEVFL",
      show_labels = "hidden",
      section_div = " ",
      afun = a_freq_j,
      extra_args = list(
        val = "Y",
        label = "Subjects with evaluable samples~[super c]",
        .stats = "count_unique"
      )
    ) |>
    split_rows_by(
      var = "AVISIT_ATPT",
      split_label = "Time Point",
      label_pos = "topleft",
      section_div = " "
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
          if (add_interquartile_range) "quantiles" else NULL
        ),
        .labels = c(
          n = "N",
          mean_sd = "Mean (SD)",
          median = "Median",
          range = "Min, max",
          quantiles = "Interquartile range"
        ),
        .formats = c(
          n = jjcsformat_xx("xx"),
          mean_sd = format_sigfig_j(3, format = "xx (xx)"),
          median = format_sigfig_j(3, format = "xx"),
          range = format_sigfig_j(3, format = "(xx, xx)"),
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
          quantiles = 1
        )
      )
    )
}


lyt_ada <- basic_table() |>
  split_cols_by(
    var = "BLANK_HEADER",
    nested = FALSE
  ) |>
  split_cols_by(
    var = "ADA_STATUS",
    split_fun = keep_split_levels(ada_status_levels),
    show_colcounts = TRUE,
    colcount_format = "N=xx"
  ) |>
  lyt_analyze()

tbl_ada <- build_table(lyt_ada, df = adpc_ada, alt_counts_df = adslx_ada, round_type = "sas")

# Table 2: Titer columns under spanning header
lyt_titer <- basic_table() |>
  split_cols_by(
    var = "TITER_HEADER",
    nested = FALSE
  ) |>
  split_cols_by(
    var = "ADA_STATUS",
    split_fun = keep_split_levels(titer_levels),
    show_colcounts = TRUE,
    colcount_format = "N=xx"
  ) |>
  lyt_analyze()

tbl_titer <- build_table(lyt_titer, df = adpc_titer, alt_counts_df = adslx_titer, round_type = "sas")

# Combine both tables
result <- cbind_rtables(tbl_ada, tbl_titer)

# Post-cbind: fix indentation lost during cbind
# Bump each AVISIT_ATPT subtree (time point label + its children) by 1
for (lv in levels(adpc_ada$AVISIT_ATPT)) {
  tryCatch(
    {
      path <- c("AVISIT_ATPT", lv)
      indent_mod(tt_at_path(result, path)) <- indent_mod(tt_at_path(result, path)) + 1L
    },
    error = function(e) NULL
  )
  # N row should be one level less indented than the other stats
  tryCatch(
    {
      path <- c("AVISIT_ATPT", lv, "AVAL", "n")
      indent_mod(tt_at_path(result, path)) <- indent_mod(tt_at_path(result, path)) - 1L
    },
    error = function(e) NULL
  )
  # N row should be one level less indented than the other stats
  tryCatch(
    {
      path <- c("AVISIT_ATPT", lv, "AVAL")
      indent_mod(tt_at_path(result, path)) <- indent_mod(tt_at_path(result, path)) + 1L
    },
    error = function(e) NULL
  )
}

# Create section dividers for each timepoint. Due to cbind intial section division removed.
section_div_at_path(result, "IMEVFL") <- " "
for (lv in levels(adpc_ada$AVISIT_ATPT)) {
  tryCatch(
    section_div_at_path(result, c("AVISIT_ATPT", lv)) <- " ",
    error = function(e) NULL
  )
}


################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table:
################################################################################


colwidth <- c(64, 31, 31, 31, 31, 31, 31)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "landscape")
