library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSIDEM02"
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
    ),
    COUNTRY = factor(
      case_when(COUNTRY == "USA" ~ "United States", TRUE ~ NA_character_)
    )
  )

################################################################################
# Further script level parameters, after having read in main data
################################################################################

demog_vars <- c("REGION1", "COUNTRY", "SITEID")

################################################################################
# Process data:
################################################################################

# filter and restrict to population of interest
adsl <- adsl |>
  filter(.data[[popfl]] == "Y") |>
  select(
    USUBJID,
    starts_with("TRT01"),
    all_of(unique(c(demog_vars)))
  )


adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

# to ensure alphabetical ordering, as COUNTRY is factor with order according COUNTRY levels, which is alphabetical on 3-letter code
adsl$REGION1 <- factor(
  as.character(adsl$REGION1),
  levels = sort(unique(as.character(adsl$REGION1)))
)
adsl$COUNTRY <- factor(
  as.character(adsl$COUNTRY),
  levels = sort(unique(as.character(adsl$COUNTRY)))
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

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) |>
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) |>
  split_cols_by(trtvar) |>
  add_overall_col("Total") |>
  split_rows_by(
    "REGION1",
    split_label = "Region",
    split_fun = trim_levels_in_group("COUNTRY"),
    label_pos = "topleft",
    section_div = " "
  ) |>
  summarize_row_groups("REGION1", cfun = a_freq_j, extra_args = list(.stats = "count_unique_fraction")) |>
  split_rows_by(
    "COUNTRY",
    split_label = "Country/Territory",
    split_fun = trim_levels_in_group("SITEID"),
    label_pos = "topleft",
    section_div = " "
  ) |>
  summarize_row_groups("COUNTRY", cfun = a_freq_j, extra_args = list(.stats = "count_unique_fraction")) |>
  analyze_vars(
    "SITEID",
    denom = "N_col",
    .stats = "count_fraction",
    .formats = c("count_fraction" = jjcsformat_count_fraction)
  ) |>
  append_topleft("    Site, n (%)")

result <- build_table(lyt, adsl, round_type = "sas")

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, titles)

################################################################################
# Convert to tbl file and output table:
################################################################################


colwidth <- c(30, 23, 23, 23, 25)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid)
