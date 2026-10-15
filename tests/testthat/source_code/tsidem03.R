library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSIDEM03"
fileid <- write_path(opath, tblid)
titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

popfl <- "FASFL"
trtvar <- "TRT01P"
ctrl_grp <- "Placebo"

################################################################################
# Initial Read in of adsl dataset
################################################################################

adsl <- adsl_jnj |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c("Xanomeline Low Dose", "Xanomeline High Dose", "Placebo")
    )
  ) |>
  filter(.data[[popfl]] == "Y")

# Identify the STRAT columns
stratd_cols <- grep("^STRAT\\d+D$", names(adsl), value = TRUE)

# Sort them in numeric order
stratd_cols <- stratd_cols[order(as.numeric(gsub("\\D", "", stratd_cols)))]

#to create labels list
stratd_labels <- lapply(stratd_cols, function(x) attr(adsl[[x]], 'label'))

# To pull labels of STRATxD variables from ADSL
stratr_cols <- sub(".$", "R", stratd_cols)

strat_cols <- c(stratd_cols, stratr_cols)

# to ensure alphabetical ordering
for (i in seq_along(strat_cols)) {
  adsl[[i]] <- factor(
    as.character(adsl[[i]]),
    levels = sort(unique(as.character(adsl[[i]])))
  )
}

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

################################################################################
# Define layout and build table:
################################################################################

lyt <- basic_table(show_colcounts = TRUE, colcount_format = "N=xx") |>
  split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) |>
  split_cols_by(trtvar) |>
  add_overall_col("Total") |>
  append_topleft("Stratification Factor \n \n  Category, n (%)") |>
  ### analyze STRATxR variables
  analyze(
    vars = stratr_cols,
    var_labels = stratd_labels,
    afun = a_summary,
    extra_args = list(
      .stats = c("n", "count_fraction"),
      .labels = c("n" = "N"),
      .formats = c(0L, n = jjcsformat_xx("xx"), "count_fraction" = jjcsformat_count_fraction),
      .indent_mods = c("n" = 0L, "count_fraction" = 1L)
    ),
    section_div = " "
  )

result <- build_table(lyt, adsl, round_type = "sas")

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, titles)

################################################################################
# Convert to tbl file and output table:
################################################################################


colwidth <- c(59, 21, 21, 21, 23)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid)
