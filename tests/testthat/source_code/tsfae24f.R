################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsfae24f.R
## R version:                 4.2.1
## Short Description:         Program to create tsfae24f: Subjects with Treatment-emergent
##                            Adverse Events by SOC/PT and Onset Time
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      26JAN2024
## Input:                     adsl.RDS, adae.RDS
## Output:                    tsfae24f.rtf
## Remarks:
##
## Modification History:
##  Rev #:
##  Modified By:
##  Reporting Effort:
##  Date:
##  Description:
################################################################################

################################################################################
# Prep environment:
################################################################################

library(envsetup)
library(tern)
library(forcats)
library(dplyr)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSFAE24f"
fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01A"
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


################################################################################
# Process data:
# Create variables for column spanning headers:
# - colspan_trt: Used to generate the "Active Study Agent" spanning header.
# - ACAT1: Month category for treatment discontinuation/completion.
# Filter AE dataframe to include 1st occurenace of a given PT.
################################################################################

adsl <- adsl_jnj %>%
  filter(!!rlang::sym(popfl) == "Y") %>%
  create_colspan_var(
    non_active_grp = "Placebo",
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = trtvar
  ) %>%
  mutate(
    months = (TRTEDY + 30) / 30.4375,
    ACAT1 = case_when(
      months <= 3 ~ "Within 3 months",
      months > 3 & months <= 6 ~ "4 to 6 months",
      months > 6 & months <= 9 ~ "7 to 9 months",
      months > 9 & months <= 12 ~ "10 to 12 months",
      months > 12 ~ "Beyond 13 months",
      .default = NA_character_
    ),
    ACAT1 = factor(
      ACAT1,
      levels = c(
        "Within 3 months",
        "4 to 6 months",
        "7 to 9 months",
        "10 to 12 months",
        "Beyond 13 months"
      )
    )
  ) %>%
  select(
    USUBJID,
    !!rlang::sym(popfl),
    !!rlang::sym(trtvar),
    colspan_trt,
    TRTEDY,
    ACAT1
  )

adae <- adae_jnj %>%
  filter(TRTEMFL == "Y") %>%
  select(
    USUBJID,
    TRTEMFL,
    AEBODSYS,
    AEDECOD,
    ASTDT,
    ACAT1,
    AOCCFL,
    AOCCPFL,
    AOCCSFL
  ) %>%
  mutate(
    ACAT1 = forcats::fct_na_value_to_level(factor(
      ACAT1,
      levels = c(
        "Within 3 months",
        "4 to 6 months",
        "7 to 9 months",
        "10 to 12 months",
        "Beyond 13 months"
      )
    ))
  ) %>%
  arrange(USUBJID, AEBODSYS, AEDECOD, ASTDT)

adae <- inner_join(adae, adsl %>% select(-ACAT1), by = c("USUBJID"))

adae$TRTEMFL <- factor(adae$TRTEMFL)

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

# This df generates facets with cumulative counts for .N_col. Note: These extra
# records must then be removed from the numerator by a_freq_combos_j function.
combodf <- tribble(
  ~valname         , ~label             , ~levelcombo                                                                                                , ~exargs ,
  "Total"          , "Total"            , c("Within 3 months", "4 to 6 months", "7 to 9 months", "10 to 12 months", "Beyond 13 months", "(Missing)") , list()  ,
  "Thru 3 months"  , "Within 3 months"  , c("Within 3 months", "4 to 6 months", "7 to 9 months", "10 to 12 months", "Beyond 13 months")              , list()  ,
  "Thru 6 months"  , "4 to 6 months"    , c("4 to 6 months", "7 to 9 months", "10 to 12 months", "Beyond 13 months")                                 , list()  ,
  "Thru 9 months"  , "7 to 9 months"    , c("7 to 9 months", "10 to 12 months", "Beyond 13 months")                                                  , list()  ,
  "Thru 12 months" , "10 to 12 months"  , c("10 to 12 months", "Beyond 13 months")                                                                   , list()  ,
  "Over 13 months" , "Beyond 13 months" , c("Beyond 13 months")                                                                                      , list()
)


extra_args1 <- list(
  combosdf = combodf,
  filter_var = "ACAT1",
  do_not_filter = "Total",
  .stats = "count_unique_fraction"
)

lyt <- basic_table(top_level_section_div = " ") %>%
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) %>%
  split_cols_by(trtvar) %>%
  split_cols_by(
    "ACAT1",
    split_fun = add_combo_levels(
      combosdf = combodf,
      trim = FALSE,
      keep_levels = combodf$valname
    )
  ) %>%
  analyze(
    popfl,
    show_labels = "hidden",
    afun = a_freq_j,
    extra_args = list(
      label = "Analysis set: Safety",
      .stats = "n_altdf"
    )
  ) %>%
  analyze(
    "TRTEMFL",
    nested = FALSE,
    show_labels = "hidden",
    afun = a_freq_combos_j,
    extra_args = append(
      extra_args1,
      list(
        val = "Y",
        label = "Subjects with >= 1 AE",
        flag_var = NULL
      )
    )
  ) %>%
  split_rows_by(
    "AEBODSYS",
    split_label = "System Organ Class",
    label_pos = "topleft",
    split_fun = trim_levels_in_group("AEDECOD"),
    section_div = c(" ")
  ) %>%
  summarize_row_groups(
    "AEBODSYS",
    cfun = a_freq_combos_j,
    extra_args = append(
      extra_args1,
      list(flag_var = NULL)
    )
  ) %>%
  analyze(
    "AEDECOD",
    afun = a_freq_combos_j,
    extra_args = append(
      extra_args1,
      list(flag_var = NULL)
    )
  ) %>%
  append_topleft("  Preferred Term, n (%)")

result <- build_table(lyt, adae, alt_counts_df = adsl)

################################################################################
# Prune & sort results
################################################################################

result <- safe_prune_table(result, prune_func = bspt_pruner())

result <- result %>%
  sort_at_path(
    path = c("root", "AEBODSYS"),
    scorefun = jj_complex_scorefun(colpath = "Total")
  ) %>%
  sort_at_path(
    path = c("root", "AEBODSYS", "*", "AEDECOD"),
    scorefun = jj_complex_scorefun(colpath = "Total")
  )

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table:
################################################################################

tt_to_tlgrtf( 
  result,
  file = fileid,
  orientation = "landscape",
  nosplitin = list(cols = c(trtvar))
)
