################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsiex01.R
## R version:                 4.2.1
## junco version:             1.0
## Short Description:         Program to create tsiex01: Duration of Treatment
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      08 Feb 2024
## Input:                     ADSL, ADEXSUM
## Output:                    TSIEX01.rtf
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
# - Define parameter code needed for duration of treatment
# - Conversion to get the unit of PARAMCD selected for treatment
#     (i.e 1 if original PARAMCD unit is days, 30.4375 if original PARAMCD unit is in months)
# - Define levels which will control ordering for AVALCAT1 in the table
# - Choose whether or not you want to present a combined active treatment column (default=TRUE)
# - Define how to create combined treatment columns (if required)
################################################################################

tblid <- "TSIEX01"
fileid <- write_path(opath, tblid)
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)


trtvar <- "TRT01A"
popfl <- "SAFFL"
ctrl_grp <- "Placebo"

comp_btw_group <- TRUE
ancova <- TRUE

trtdur <- "TRTDURM"
daysconv <- 30.4375
catlevels <- c(
  "0 to <3 months",
  "3 to <6 months",
  "6 to <9 months",
  "9 to <12 months",
  "12 to <15 months",
  "15 to <18 months",
  "18 to <21 months",
  "21 to <24 months",
  "24 to <27 months",
  "27 to <30 months",
  "30 to <33 months",
  "33 to <36 months",
  "36 to <39 months"
)

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
  select(STUDYID, USUBJID, all_of(trtvar), all_of(popfl))

# If AVISIT is not present in ADEXSUM than create 'Overall' as the Visit, which is used
# for the filtering AVISIT records if it does exist
adexsum <- adexsum_jnj %>%
  mutate(VISIT = if (exists("AVISIT")) AVISIT else "Overall") %>%
  filter(PARAMCD == trtdur & !is.na(AVAL) & VISIT == "Overall") %>%
  mutate(
    CRIT0FL = "Y",
    CRIT0 = as.factor("Any duration (at least 1 dose)")
  ) %>%
  select(USUBJID, PARAMCD, AVAL, AVALCAT1, starts_with("CRIT"))

adexsum$CRIT0FL <- factor(adexsum$CRIT0FL, levels = c("Y", "N"))

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == "Placebo", " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

adsl$diff_header <- "Difference in Means (95% CI)"
adsl$diff_label <- paste(adsl[[trtvar]], paste("vs", ctrl_grp))

# join data together
ex <- adexsum %>% inner_join(., adsl, by = c("USUBJID"))

# Keep only columns with some data in which will remove any unwanted CRITy variables
ex <- ex[, colSums(is.na(ex)) < nrow(ex)]

# Work out how many CRITy vars (ignoring CRIT0 we created) we have left
excritvars <- ex %>%
  select(num_range("CRIT", 1:99))

countcritvars <- length(names(excritvars))

# Drop unwanted levels for all CRITy variables you have remaining in ex and also for AVALCAT1

for (i in 1:countcritvars) {
  variable_name <- paste0("CRIT", i)
  ex[[variable_name]] <- droplevels(ex[[variable_name]])
}

# drop unwanted levels from AVALCAT1 and assign levels from specified section at the top of script
ex$AVALCAT1 <- droplevels(ex$AVALCAT1)
ex$AVALCAT1 <- factor(ex$AVALCAT1, levels = catlevels)

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)
ref_path <- c("colspan_trt", "", trtvar, ctrl_grp)

###################################################################
# Create label_map to be utilized in the Criterion variables (CRIT)
###################################################################

ex_lblmap <- ex %>%
  select(-CRIT0, -CRIT0FL) %>%
  tidyr::pivot_longer(
    cols = starts_with("CRIT") & ends_with("FL"),
    names_to = "value",
    values_to = "CF_value"
  ) %>%
  # Create the label by selecting the corresponding values from the CRIT columns
  mutate(
    label = case_when(
      value == "CRIT1FL" ~ CRIT1,
      value == "CRIT2FL" ~ CRIT2,
      value == "CRIT3FL" ~ CRIT3,
      value == "CRIT4FL" ~ CRIT4,
      value == "CRIT5FL" ~ CRIT5,
      value == "CRIT6FL" ~ CRIT6,
      value == "CRIT7FL" ~ CRIT7
    )
  ) %>%
  select(value, label) %>%
  distinct()


################################################################################
# Define layout and build table:
################################################################################

extra_args_rr <- list(.stats = c("count_unique_fraction"), riskdiff = FALSE)

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
  split_cols_by("diff_header", nested = FALSE) %>%
  split_cols_by(
    trtvar,
    split_fun = remove_split_levels(ctrl_grp),
    labels_var = "diff_label"
  ) %>%
  analyze(
    "AVAL",
    afun = a_summarize_ex_j,
    var_labels = "Duration of treatment~[super a], ([unit])",
    show_labels = "visible",
    indent_mod = 0L,
    extra_args = list(
      daysconv = daysconv,
      ref_path = ref_path,
      ancova = ancova,
      comp_btw_group = comp_btw_group,
      variables = list(arm = trtvar, covariates = NULL),
      .formats = c(
        range = jjcsformat_xx("xx.x, xx.x"),
        "diff_mean_est_ci" = jjcsformat_xx("xx.xx (xx.xx, xx.xx)")
      )
    )
  ) %>%
  analyze(
    "AVALCAT1",
    var_labels = "Duration of treatment, n (%)",
    afun = a_freq_j,
    extra_args = extra_args_rr,
    indent_mod = 1L,
    show_labels = "visible",
    nested = FALSE
  ) %>%
  analyze(
    paste0("CRIT", 1, "FL"),
    var_labels = "Duration of treatment~[super b], n (%)",
    afun = a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(label = as.character(ex_lblmap$label[1]), val = "Y")
    ),
    indent_mod = 1L,
    show_labels = "visible",
    nested = FALSE
  )

# Add in analyze for all others CRIT variables contained in ex
for (i in 2:countcritvars) {
  lyt <- lyt %>%
    analyze(
      paste0("CRIT", i, "FL"),
      afun = a_freq_j,
      extra_args = append(
        extra_args_rr,
        list(label = as.character(ex_lblmap$label[i]), val = "Y")
      ),
      indent_mod = 2L,
      show_labels = "hidden"
    )
}

lyt <- lyt %>%
  append_topleft("Parameter")

result <- build_table(lyt, ex, alt_counts_df = adsl)

## Remove the N=xx column headers for the Difference (95% CI) columns
result <- remove_col_count(result, span_label_var = "diff_header")

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table
################################################################################
tt_to_tlgrtf(result, file = fileid, orientation = "landscape")
