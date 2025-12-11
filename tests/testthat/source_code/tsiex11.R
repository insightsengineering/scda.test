################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsiex11.R
## R version:                 4.2.1
## junco version:             1.0
## Short Description:         Program to create tsiex11: Incidence and Reasons for
##                            Treatment Modifications for Infusions
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      06 Feb 2024
## Input:                     ADSL, ADEXSUM
## Output:                    TSIEX11.rtf
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
# - Choose whether or not you want to present a combined active treatment column (default=TRUE)
# - Define how to create combined treatment columns (if required)
################################################################################

tblid <- "TSIEX11"
fileid <- write_path(opath, tblid)
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


trtvar <- "TRT01A"
popfl <- "SAFFL"
combined_colspan_trt <- TRUE
ctrl_grp <- "Placebo"

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

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)


core_ex_vars <- c(
  "USUBJID",
  "AACTPR",
  "AACTDU",
  "AADJ",
  "AADJP",
  "AVISIT",
  "AVISITN"
)

adex1 <- adex_jnj %>%
  filter(!grepl("UNSCHEDULED", AVISIT, ignore.case = TRUE)) %>%
  select(all_of(core_ex_vars))

# Convert AVISIT to sentence case and apply levels to maintain ordering
avisit_levs <- stringr::str_to_sentence(levels(adex1$AVISIT))
adex1$AVISIT <- stringr::str_to_sentence(adex1$AVISIT)
adex1$AVISIT <- factor(adex1$AVISIT, levels = avisit_levs)


adex1a <- adex1 %>%
  mutate(
    AACTx = AACTPR,
    AADJx = AADJ,
    XXX = "Prior to infusion"
  )

adex1b <- adex1 %>%
  mutate(
    AACTx = AACTDU,
    AADJx = AADJP,
    XXX = "During infusion"
  )

adex1c <- bind_rows(adex1a, adex1b)
adex1c$XXX <- factor(
  adex1c$XXX,
  levels = c("Prior to infusion", "During infusion")
)

### Overall visit

adex2 <- adex_jnj %>%
  mutate(AVISIT = "Overall") %>%
  select(all_of(core_ex_vars))

adex2a <- adex2 %>%
  mutate(
    AACTx = AACTPR,
    AADJx = AADJ,
    XXX = "Prior to infusion"
  )

adex2b <- adex2 %>%
  mutate(
    AACTx = AACTDU,
    AADJx = AADJP,
    XXX = "During infusion"
  )

adex2c <- bind_rows(adex2a, adex2b)
adex2c$XXX <- factor(
  adex2c$XXX,
  levels = c("Prior to infusion", "During infusion")
)

adex <- bind_rows(adex1c, adex2c)

# control levels of AVISIT
adex$AVISIT <- factor(adex$AVISIT, levels = c("Overall", levels(adex1$AVISIT)))

# control levels of new variables AACTx and AADJx
aact_levels <- c(
  "DOSE REDUCED COMPARED TO PRIOR INFUSION",
  "INFUSION DELAYED WITHIN THE CYCLE",
  "INFUSION RATE DECREASED COMPARED TO PRIOR INFUSION",
  "INFUSION SKIPPED (AND NOT MADE UP)",
  "STUDY DRUG PERMANENTLY DISCONTINUED",
  "INFUSION ABORTED",
  "INFUSION INTERRUPTED",
  "INFUSION RATE INCREASED"
)

aact_labels <- c(
  "Dose reduced",
  "Infusion delayed",
  "Infusion rate decreased",
  "Infusion skipped",
  "Study agent permanently discontinued",
  "Infusion aborted",
  "Infusion interrupted",
  "Infusion rate increased"
)

adex$AACTx <- factor(
  as.character(adex$AACTx),
  levels = aact_levels,
  labels = aact_labels
)

adex$AADJx <- factor(adex$AADJx, levels = levels(c(adex$AADJ, adex$AADJP)))

### Note the following levels are not used on the table, not sure if this is OK ---
### this is out of scope for template script
### how standard is this table and the levels of the variables on the current dataset + standard ADaM???
setdiff(levels(adex$AACTPR), aact_levels)
setdiff(levels(adex$AACTDU), aact_levels)


# join data together
ex <- adex %>% left_join(., adsl, by = c("USUBJID"))

### limit visits to some for testing
ex2 <- ex %>%
  filter(AVISIT %in% levels(ex$AVISIT)[1:4]) %>%
  mutate(AVISIT = factor(as.character(AVISIT), levels = levels(ex$AVISIT)[1:4]))


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
ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)


split_map <- tribble(
  ~XXX                , ~AACTx                                 ,
  "Prior to infusion" , "Dose reduced"                         ,
  "Prior to infusion" , "Infusion delayed"                     ,
  "Prior to infusion" , "Infusion rate decreased"              ,
  "Prior to infusion" , "Infusion skipped"                     ,
  "Prior to infusion" , "Study agent permanently discontinued" ,
  "During infusion"   , "Infusion aborted"                     ,
  "During infusion"   , "Infusion interrupted"                 ,
  "During infusion"   , "Infusion rate increased"
)


extra_args1 <- list(
  .stats = "count_unique_fraction",
  denom = "n_parentdf",
  denom_by = "AVISIT",
  drop_levels = TRUE
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
  split_rows_by(
    "AVISIT",
    split_label = "Time Point",
    split_fun = drop_split_levels,
    label_pos = "topleft",
    child_labels = "visible",
    indent_mod = 0L,
    section_div = c(" ")
  ) %>%
  split_rows_by(
    "XXX",
    split_fun = keep_split_levels(c(
      "Prior to infusion",
      "During infusion"
    )),
    section_div = " ",
    child_labels = "visible"
  ) %>%
  summarize_row_groups(
    "XXX",
    cfun = a_freq_j,
    extra_args = list(.stats = "n_df", label = "N")
  ) %>%
  split_rows_by(
    "AACTx",
    split_fun = trim_levels_to_map(map = split_map),
    section_div = " "
  ) %>%
  summarize_row_groups("AACTx", cfun = a_freq_j, extra_args = extra_args1) %>%
  analyze("AADJx", afun = a_freq_j, extra_args = extra_args1) %>%
  append_topleft("  Time Relative to Infusion") %>%
  append_topleft("    Modification, n (%)")

result <- build_table(lyt, ex, alt_counts_df = adsl)

################################################################################
# Prune table to remove all the rows with 0 counts or no data to report across all sections
################################################################################

result <- prune_table(result, prune_func = prune_empty_level)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table
################################################################################
tt_to_tlgrtf(result, file = fileid, orientation = "portrait")
