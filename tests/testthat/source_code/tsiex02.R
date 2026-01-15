################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsiex02.R
## R version:                 4.2.1
## junco version:             1.0
## Short Description:         Program to create tsiex02: Study Treatment Administration
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      05 Feb 2024
## Input:                     ADSL, ADEXSUM
## Output:                    TSIEX02.rtf
## Remarks:                   Template R script version using rtables framework
##
## Modification History:
##  Rev #:                    1
##  Modified By:
##  Reporting Effort:
##  Date:
##  Description:
################################################################################

################################################################################
# Prep Environment
################################################################################

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
# - Define levels which will control ordering for AVALCAT variables in the table
# - Choose whether or not you want to present a combined active treatment column (default=TRUE)
# - Define how to create combined treatment columns (if required)
################################################################################

tblid <- "TSIEX02"
fileid <- write_path(opath, tblid)
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


trtvar <- "TRT01A"
popfl <- "SAFFL"

catlevels <- c("20 mg", "30 mg", "40 mg")

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
adsl <- adsl_jnj %>%
  filter(!!rlang::sym(popfl) == "Y") %>%
  select(USUBJID, all_of(trtvar), all_of(popfl))

# If AVISIT is not present in ADEXSUM than create 'Overall' as the Visit, which is used
# for the filtering AVISIT records if it does exist
adexsum <- adexsum_jnj %>%
  mutate(VISIT = if (exists("AVISIT")) AVISIT else "Overall") %>%
  filter(
    PARAMCD %in%
      c("DOSEDAYS", "CUMDOSE", "MEANDDI", "MEANDD", "MODEDD", "FINDD") &
      !is.na(AVAL) &
      VISIT == "Overall"
  ) %>%
  mutate(
    AVAL1 = case_when(PARAMCD == "DOSEDAYS" ~ AVAL),
    AVAL2 = case_when(PARAMCD == "CUMDOSE" ~ AVAL),
    AVAL3 = case_when(PARAMCD == "MEANDDI" ~ AVAL),
    AVAL4 = case_when(PARAMCD == "MEANDD" ~ AVAL),
    AVAL5 = case_when(PARAMCD == "MODEDD" ~ AVAL),
    AVALCAT5 = case_when(PARAMCD == "MODEDD" ~ AVALCAT1),
    AVAL6 = case_when(PARAMCD == "FINDD" ~ AVAL),
    AVALCAT6 = case_when(PARAMCD == "FINDD" ~ AVALCAT1)
  ) %>%
  select(
    STUDYID,
    USUBJID,
    PARAMCD,
    AVAL1,
    AVAL2,
    AVAL3,
    AVAL4,
    AVAL5,
    AVAL6,
    AVALCAT5,
    AVALCAT6
  )

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == "Placebo", " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

# join data together
ex <- adexsum %>% inner_join(., adsl, by = c("USUBJID"))

# Drop unwanted levels for AVALCAT5 and AVALCAT6 and set levels defined in top section of script

ex$AVALCAT5 <- droplevels(ex$AVALCAT5)
ex$AVALCAT5 <- factor(ex$AVALCAT5, levels = catlevels)
ex$AVALCAT6 <- droplevels(ex$AVALCAT6)
ex$AVALCAT6 <- factor(ex$AVALCAT6, levels = catlevels)

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

extra_args1 <- list(
  .stats = "count_unique_fraction",
  denom = "n_df"
)


lyt <- rtables::basic_table(
  top_level_section_div = " ",
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) %>%
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  )

if (combined_colspan_trt == TRUE) {
  lyt <- lyt %>%
    split_cols_by(trtvar, split_fun = mysplit)
} else {
  lyt <- lyt %>%
    split_cols_by(trtvar)
}

lyt <- lyt %>%
  analyze(
    "AVAL1",
    table_names = "AVAL1x",
    var_labels = "Total dosing days of treatment (excluding days off treatment)~[super a]",
    show_labels = "visible",
    indent_mod = 0L,
    afun = function(x) {
      list(
        "N" = rcell(length(x), format = jjcsformat_xx("xx"))
      )
    }
  ) %>%
  analyze(
    "AVAL1",
    nested = TRUE,
    var_labels = "Total dosing days of treatment (excluding days off treatment)~[super a]",
    show_labels = "hidden",
    indent_mod = 2L,
    afun = function(x) {
      list(
        "Mean (SD)" = rcell(
          c(mean(x), sd(x)),
          format = jjcsformat_xx("xx.x (xx.xx)")
        ),
        "Median" = rcell(median(x), format = jjcsformat_xx("xx.x")),
        "Min, max" = rcell(
          c(min(x), max(x)),
          format = jjcsformat_xx("xx., xx.")
        ),
        "Interquartile range" = rcell(
          c(quantile(x, c(0.25, 0.75), type = 2)),
          format = jjcsformat_xx("xx.x, xx.x")
        )
      )
    }
  ) %>%
  analyze(
    "AVAL2",
    table_names = "AVAL2x",
    nested = FALSE,
    var_labels = "Cumulative dose ([unit])",
    show_labels = "visible",
    indent_mod = 0L,
    afun = function(x) {
      list(
        "N" = rcell(length(x), format = jjcsformat_xx("xx"))
      )
    }
  ) %>%
  analyze(
    "AVAL2",
    nested = TRUE,
    var_labels = "Cumulative dose ([unit])",
    show_labels = "hidden",
    indent_mod = 2L,
    afun = function(x) {
      list(
        "Mean (SD)" = rcell(
          c(mean(x), sd(x)),
          format = jjcsformat_xx("xx.x (xx.xx)")
        ),
        "Median" = rcell(median(x), format = jjcsformat_xx("xx.x")),
        "Min, max" = rcell(
          c(min(x), max(x)),
          format = jjcsformat_xx("xx., xx.")
        ),
        "Interquartile range" = rcell(
          c(quantile(x, c(0.25, 0.75), type = 2)),
          format = jjcsformat_xx("xx.x, xx.x")
        )
      )
    }
  ) %>%
  analyze(
    "AVAL3",
    table_names = "AVAL3x",
    nested = FALSE,
    var_labels = "Average daily dose ([unit/day]) (including days off treatment)",
    show_labels = "visible",
    indent_mod = 0L,
    afun = function(x) {
      list(
        "N" = rcell(length(x), format = jjcsformat_xx("xx"))
      )
    }
  ) %>%
  analyze(
    "AVAL3",
    nested = TRUE,
    var_labels = "Average daily dose ([unit/day]) (including days off treatment)",
    show_labels = "hidden",
    indent_mod = 2L,
    afun = function(x) {
      list(
        "Mean (SD)" = rcell(
          c(mean(x), sd(x)),
          format = jjcsformat_xx("xx.xx (xx.xxx)")
        ),
        "Median" = rcell(median(x), format = jjcsformat_xx("xx.xx")),
        "Min, max" = rcell(
          c(min(x), max(x)),
          format = jjcsformat_xx("xx.x, xx.x")
        ),
        "Interquartile range" = rcell(
          c(quantile(x, c(0.25, 0.75), type = 2)),
          format = jjcsformat_xx("xx.xx, xx.xx")
        )
      )
    }
  ) %>%
  analyze(
    "AVAL4",
    table_names = "AVAL4x",
    nested = FALSE,
    var_labels = "Average daily dose ([unit/day]) (excluding days off treatment)",
    show_labels = "visible",
    indent_mod = 0L,
    afun = function(x) {
      list(
        "N" = rcell(length(x), format = jjcsformat_xx("xx"))
      )
    }
  ) %>%
  analyze(
    "AVAL4",
    nested = TRUE,
    var_labels = "Average daily dose ([unit/day]) (excluding days off treatment)",
    show_labels = "hidden",
    indent_mod = 2L,
    afun = function(x) {
      list(
        "Mean (SD)" = rcell(
          c(mean(x), sd(x)),
          format = jjcsformat_xx("xx.xx (xx.xxx)")
        ),
        "Median" = rcell(median(x), format = jjcsformat_xx("xx.xx")),
        "Min, max" = rcell(
          c(min(x), max(x)),
          format = jjcsformat_xx("xx.x, xx.x")
        ),
        "Interquartile range" = rcell(
          c(quantile(x, c(0.25, 0.75), type = 2)),
          format = jjcsformat_xx("xx.xx, xx.xx")
        )
      )
    }
  ) %>%
  analyze(
    "AVAL5",
    nested = FALSE,
    var_labels = "Modal daily dose ([unit/day]), n (%)",
    show_labels = "visible",
    indent_mod = 0L,
    afun = function(x) {
      list(
        "N" = rcell(length(x), format = jjcsformat_xx("xx"))
      )
    }
  ) %>%
  analyze(
    "AVALCAT5",
    afun = a_freq_j,
    extra_args = extra_args1,
    show_labels = "hidden",
    indent_mod = 2L
  ) %>%
  analyze(
    "AVAL6",
    table_names = "AVAL6x",
    nested = FALSE,
    var_labels = "Final daily dose ([unit/day])",
    show_labels = "visible",
    indent_mod = 0L,
    afun = function(x) {
      list(
        "N" = rcell(length(x), format = jjcsformat_xx("xx"))
      )
    }
  ) %>%
  analyze(
    "AVAL6",
    nested = TRUE,
    var_labels = "Final daily dose ([unit/day])",
    show_labels = "hidden",
    indent_mod = 2L,
    afun = function(x) {
      list(
        "Mean (SD)" = rcell(
          c(mean(x), sd(x)),
          format = jjcsformat_xx("xx.x (xx.xx)")
        ),
        "Median" = rcell(median(x), format = jjcsformat_xx("xx.x")),
        "Min, max" = rcell(
          c(min(x), max(x)),
          format = jjcsformat_xx("xx., xx.")
        ),
        "Interquartile range" = rcell(
          c(quantile(x, c(0.25, 0.75), type = 2)),
          format = jjcsformat_xx("xx.x, xx.x")
        )
      )
    }
  ) %>%
  analyze(
    "AVALCAT6",
    afun = a_freq_j,
    nested = FALSE,
    show_labels = "visible",
    var_labels = "Final daily dose ([unit/day]), n (%)",
    extra_args = extra_args1,
    indent_mod = 1L
  ) %>%
  append_topleft("Parameter")

result <- build_table(lyt, ex, alt_counts_df = adsl)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table
################################################################################
tt_to_tlgrtf(result, file = fileid, orientation = "portrait")
