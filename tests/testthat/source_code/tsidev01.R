library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSIDEV01"
fileid <- write_path(opath, tblid)
titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)
popfl <- "FASFL"
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"

################################################################################
# Initial Read in of adsl dataset
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

dv <- dv_jnj


################################################################################
# Further script level parameters, after having read in main data
################################################################################
## Optional: Add "REGION1" to demog_vars if regional analysis is required
demog_vars <- c("REGION1", "SITEID")
demog_displ_vars <- demog_vars

## make it named vars so that demog_vars[xx] with xx subset of vars still works
names(demog_vars) <- demog_vars
## retrieve labels
demog_labels <- formatters::var_labels(adsl)[demog_vars]

################################################################################
# Process data:
################################################################################

# filter and restrict to population of interest
adsl <- adsl |>
  filter(.data[[popfl]] == "Y") |>
  select(
    USUBJID,
    starts_with("TRT01"),
    all_of(popfl),
    all_of(unique(c(demog_vars, demog_displ_vars)))
  )

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

if ("REGION1" %in% demog_vars) {
  # to ensure alphabetical ordering according to COUNTRY variable
  adsl$REGION1 <- factor(
    as.character(adsl$REGION1),
    levels = sort(unique(as.character(adsl$REGION1)))
  )
}


dv <- dv |>
  filter(toupper(DVCAT) == "MAJOR") |>
  select(STUDYID, USUBJID, DVSEQ, DVDECOD, DVTERM, DVSTDTC, DVDECOD) |>
  mutate(
    DVSTDT = as.Date(DVSTDTC),
    HASDEVFL = "Y"
  )

adsl_dev <- adsl |>
  inner_join(dv, by = "USUBJID")

colspan_trt_map <- create_colspan_map(
  adsl_dev,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

################################################################################
# Define layout and build table:
################################################################################
extra_args <- list(
  denom = "n_altdf",
  .stats = "count_unique_fraction"
)

lyt <- basic_table(
  top_level_section_div = " ",
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) |>
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) |>
  split_cols_by(trtvar) |>
  add_overall_col("Total")
if ("REGION1" %in% demog_vars) {
  lyt <- lyt |>
    split_rows_by(
      "REGION1"
    ) |>
    summarize_row_groups(
      "REGION1",
      cfun = a_freq_j,
      extra_args = append(
        extra_args,
        list(
          label_fstr = "Region: %s",
          .labels_n = ""
        )
      )
    )
}

lyt <- lyt |>
  split_rows_by(
    "SITEID",
    section_div = " "
  ) |>
  summarize_row_groups(
    "SITEID",
    cfun = a_freq_j,
    extra_args = append(
      extra_args,
      list(
        label_fstr = "Site: %s",
        .labels_n = "",
        extrablankline = TRUE
      )
    )
  ) |>
  split_rows_by(
    "HASDEVFL",
    split_fun = drop_split_levels
  ) |>
  summarize_row_groups(
    "HASDEVFL",
    cfun = a_freq_j,
    extra_args = append(
      extra_args,
      list(
        label = "Subjects with major protocol deviations",
        .labels_n = ""
      )
    )
  ) |>
  analyze(
    "DVDECOD",
    afun = a_freq_j,
    extra_args = append(
      extra_args,
      list(
        .labels_n = ""
      )
    )
  )

result <- build_table(lyt, adsl_dev, alt_counts_df = adsl, round_type = "sas")

#########################################################################################
# Post-Processing step to sort by descending count based on the "Total" column.
# Sorting is applied hierarchically to Region, Center, and Protocol Deviation rows.
# The "Other" deviation category is explicitly forced to the bottom of the list using
# the lastcat argument. See function documentation for jj_complex_scorefun should you
# require a different sorting behavior.
#########################################################################################

sort_colpath <- c("Total", "Total")

if (nrow(adsl_dev) != 0) {
  if ("REGION1" %in% demog_vars) {
    result <- result |>
      sort_at_path(
        path = c("REGION1"),
        scorefun = jj_complex_scorefun(colpath = sort_colpath)
      ) |>
      sort_at_path(
        path = c("REGION1", "*", "SITEID"),
        scorefun = jj_complex_scorefun(colpath = sort_colpath)
      ) |>
      sort_at_path(
        path = c("REGION1", "*", "SITEID", "*", "HASDEVFL", "*", "DVDECOD"),
        scorefun = jj_complex_scorefun(
          colpath = sort_colpath,
          lastcat = "count_unique_fraction.Other"
        )
      )
  } else {
    result <- result |>
      sort_at_path(
        path = c("SITEID"),
        scorefun = jj_complex_scorefun(colpath = sort_colpath)
      ) |>
      sort_at_path(
        path = c("SITEID", "*", "HASDEVFL", "*", "DVDECOD"),
        scorefun = jj_complex_scorefun(
          colpath = sort_colpath,
          lastcat = "count_unique_fraction.Other"
        )
      )
  }
}

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, titles)

################################################################################
# Convert to tbl file and output table:
################################################################################


colwidth <- c(64, 21, 21, 21, 21)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid)
