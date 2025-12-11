################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsiex03.R
## R version:                 4.2.1
## junco version:             1.0
## Short Description:         Program to create tsiex03: Study Treatment Administration
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      05 Feb 2024
## Input:                     ADSL, ADEXSUM
## Output:                    TSIEX03.rtf
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
# - Define levels which will control ordering for AVALCAT1 in the table
# - Choose whether or not you want to present a combined active treatment column (default=TRUE)
# - Define how to create combined treatment columns (if required)
################################################################################

tblid <- "TSIEX03"
fileid <- write_path(opath, tblid)
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


trtvar <- "TRT01A"
popfl <- "SAFFL"

catlevels <- c("1 to <10", "10 to <20", ">=20")

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
adexsum1 <- adexsum_jnj %>%
  mutate(VISIT = if (exists("AVISIT")) AVISIT else "Overall") %>%
  filter(PARAMCD == "TNUMDOS" & !is.na(AVAL) & VISIT == "Overall") %>%
  mutate(AVAL1 = AVAL) %>%
  select(STUDYID, USUBJID, PARAMCD, AVAL1, AVALCAT1, starts_with("CRIT"))

adexsum2 <- adexsum_jnj %>%
  mutate(VISIT = if (exists("AVISIT")) AVISIT else "Overall") %>%
  filter(PARAMCD == "CUMDOSE" & !is.na(AVAL) & VISIT == "Overall") %>%
  mutate(AVAL2 = AVAL) %>%
  select(STUDYID, USUBJID, PARAMCD, AVAL2)

adexsum <- bind_rows(adexsum1, adexsum2) %>%
  select(STUDYID, USUBJID, PARAMCD, AVAL1, AVAL2, AVALCAT1, starts_with("CRIT"))

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == "Placebo", " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

# join data together
ex <- adexsum %>% inner_join(., adsl, by = c("USUBJID"))

# Keep only columns with some data in which will remove any unwanted CRITy variables
ex <- ex[, colSums(is.na(ex)) < nrow(ex)]

# Work out how many CRITy vars we have left
excritvars <- ex %>%
  select(num_range("CRIT", 1:99))

countcritvars <- length(names(excritvars))

# Drop unwanted levels for all CRITy variables you have remaining in ex and also for AVALCAT1

critlbls <- list()
for (i in 1:countcritvars) {
  variable_name <- paste0("CRIT", i)
  ex[[variable_name]] <- droplevels(ex[[variable_name]])
  critlbls[[i]] <- unique(as.character(ex[[variable_name]][
    !is.na(ex[[variable_name]])
  ]))
}

# drop unwanted levels from AVALCAT1 and assign levels from specified section at the top of script
ex$AVALCAT1 <- droplevels(ex$AVALCAT1)
ex$AVALCAT1 <- factor(ex$AVALCAT1, levels = catlevels)

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

extra_args2 <- list(
  denom = "n_df",
  .stats = "count_unique_fraction"
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
    var_labels = "Total number of administrations",
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
    var_labels = "Total number of administrations",
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
    "AVALCAT1",
    nested = FALSE,
    afun = a_freq_j,
    var_labels = "Total number of administrations, n (%)",
    extra_args = extra_args1,
    show_labels = "visible",
    indent_mod = 1L
  ) %>%
  analyze(
    "CRIT1",
    afun = a_freq_j,
    nested = FALSE,
    show_labels = "visible",
    var_labels = "Total number of administrations~[super a], n (%)",
    extra_args = extra_args1,
    indent_mod = 1L
  )


# Add in analzye for all remaining CRIT variables contained in ex
for (i in 2:countcritvars) {
  lyt <- lyt %>%
    analyze(
      paste0("CRIT", i, "FL"),
      afun = a_freq_j,
      extra_args = list(
        val = "Y",
        label = critlbls[[i]],
        denom = "n_df",
        .stats = c("count_unique_fraction")
      ),
      indent_mod = 2L,
      show_labels = "hidden"
    )
}


lyt <- lyt %>%
  analyze(
    "AVAL2",
    table_names = "AVAL2x",
    var_labels = "Cumulative dose ([units])",
    nested = FALSE,
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
    var_labels = "Cumulative dose ([units])",
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
