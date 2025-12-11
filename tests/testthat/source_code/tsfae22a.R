###############################################################################################
## Original Reporting Effort: Standards
## Program Name: tsfae22a.R
## R Version: 4.2.1
## junco Version: 1.0
## Short Description: Program to create tsfae22a: TEAEs: SOC / PT by: Race
## Author:                    Johnson & Johnson Innovative Medicine
## Date: 18 Dec 2023
## Input: ADSL, ADAE.
## Output: TSFAE22a.rtf
## Remarks: Template R script version using rtables framework
##
## Modification History:
## Rev #: 1
## Modified By:
## Reporting effort:
## Date:
## Description:
###############################################################################################

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
# - Define column widths to help with desired page splitting
################################################################################

tblid <- "TSFAE22a"
fileid <- write_path(opath, tblid)
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


trtvar <- "TRT01A"
popfl <- "SAFFL"
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

adsl <- adsl_jnj %>%
  filter(!!rlang::sym(popfl) == "Y") %>%
  select(STUDYID, USUBJID, all_of(trtvar), all_of(popfl), RACE)

adae <- adae_jnj %>%
  filter(TRTEMFL == "Y") %>%
  select(USUBJID, TRTEMFL, AEBODSYS, AEDECOD, RACE)

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == "Placebo", " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = "Placebo",
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

# Add total for Race - adsl
totalrace1 <- adsl %>%
  filter(RACE != "UNKNOWN" & !is.na(RACE)) %>%
  mutate(RACE = "Total")

adsl <- bind_rows(totalrace1, adsl)

adsl <- adsl %>%
  mutate(
    RACEcat = case_when(
      RACE == "Total" ~ "Total",
      RACE == "WHITE" ~ "White",
      RACE == "BLACK OR AFRICAN AMERICAN" ~ "Black",
      RACE == "ASIAN" ~ "Asian",
      RACE != "UNKNOWN" & !is.na(RACE) ~ "Other"
    )
  ) %>%
  filter(RACEcat %in% c("Total", "White", "Black", "Asian", "Other")) %>%
  select(-RACE)

adsl$spanheader <- factor(
  ifelse(adsl$RACEcat == "Total", " ", "Race"),
  levels = c(" ", "Race")
)

adsl$RACEcat <- factor(
  adsl$RACEcat,
  levels = c("Total", "White", "Black", "Asian", "Other")
)

# Add total for Race - adae
totalrace2 <- adae %>%
  filter(RACE != "UNKNOWN" & !is.na(RACE)) %>%
  mutate(RACE = "Total")

adae <- bind_rows(totalrace2, adae)

adae <- adae %>%
  mutate(
    RACEcat = case_when(
      RACE == "Total" ~ "Total",
      RACE == "WHITE" ~ "White",
      RACE == "BLACK OR AFRICAN AMERICAN" ~ "Black",
      RACE == "ASIAN" ~ "Asian",
      RACE != "UNKNOWN" & !is.na(RACE) ~ "Other"
    )
  ) %>%
  filter(RACEcat %in% c("Total", "White", "Black", "Asian", "Other")) %>%
  select(-RACE)

adae$RACEcat <- factor(
  adae$RACEcat,
  levels = c("Total", "White", "Black", "Asian", "Other")
)

# join data together
ae <- left_join(adsl, adae, by = c("USUBJID", "RACEcat"))

################################################################################
# Define layout and build table:
################################################################################

extra_args_1 <- list(
  denom = "n_altdf",
  .stats = c("count_unique_fraction")
)


extra_args_2 <- list(
  denom = "n_altdf",
  .stats = c("count_unique")
)


lyt <- basic_table(
  top_level_section_div = " ",
  show_colcounts = FALSE
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
  split_cols_by("spanheader", split_fun = trim_levels_in_group("RACEcat")) %>%
  split_cols_by("RACEcat") %>%
  analyze(
    popfl,
    afun = a_freq_j,
    show_labels = "hidden",
    section_div = c(" "),
    extra_args = append(
      extra_args_2,
      list(
        label = "Analysis set: Safety",
        val = "Y",
        section_div = c(" ")
      )
    )
  ) %>%
  analyze(
    "TRTEMFL",
    afun = a_freq_j,
    show_labels = "hidden",
    extra_args = append(
      extra_args_1,
      list(
        label = "Subjects with >=1 AE",
        val = "Y",
        section_div = c(" ")
      )
    )
  ) %>%
  split_rows_by(
    "AEBODSYS",
    split_label = "System Organ Class",
    split_fun = trim_levels_in_group("AEDECOD"),
    label_pos = "topleft",
    section_div = c(" "),
    nested = FALSE
  ) %>%
  summarize_row_groups(
    "AEBODSYS",
    cfun = a_freq_j,
    extra_args = extra_args_1
  ) %>%
  analyze("AEDECOD", afun = a_freq_j, extra_args = extra_args_1) %>%
  append_topleft("  Preferred Term, n (%)")

result <- build_table(lyt, ae, alt_counts_df = adsl)

#########################################################################################
# Post-Processing step to sort by descending count on chosen active treatment columns.
# For this table we can use a defined colpath so it takes the appropriate sub-column ("Total")
# for the last active treatment group/combined and use this for its sort order.
# If you only have 1 active treatment arm, consider using jj_complex_scorefun(spanningheadercolvar = NA, usefirstcol = TRUE)
# See function documentation for jj_complex_scorefun should your require a different sorting behavior.
#########################################################################################

# col_paths_summary(result)

if (length(adae$TRTEMFL) != 0) {
  result <- sort_at_path(
    result,
    c("root", "AEBODSYS"),
    scorefun = jj_complex_scorefun(
      colpath = c(
        "colspan_trt",
        "Active Study Agent",
        trtvar,
        "Combined",
        "spanheader",
        " ",
        "RACEcat",
        "Total"
      )
    )
  )
  result <- sort_at_path(
    result,
    c("root", "AEBODSYS", "*", "AEDECOD"),
    scorefun = jj_complex_scorefun(
      colpath = c(
        "colspan_trt",
        "Active Study Agent",
        trtvar,
        "Combined",
        "spanheader",
        " ",
        "RACEcat",
        "Total"
      )
    )
  )
}

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table
################################################################################

tt_to_tlgrtf( 
  result,
  file = fileid,
  orientation = "portrait",
  label_width_ins = 1.5,
  nosplitin = list(cols = c(trtvar))
)
