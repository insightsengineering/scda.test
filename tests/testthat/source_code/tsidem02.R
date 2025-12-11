################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsidem02
## R version:                 4.2.1
## Short Description:         Program to create tsidem02: Subjects by Region, Country, and Site
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      30JAN2024
## Input:                     adsl.RDS
## Output:                    tsidem02.rtf
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

popfls <- c("SAFFL", "ITTFL", "FASFL")
popfl <- popfls[1]
trtvar <- "TRT01P"
ctrl_grp <- "Placebo"

################################################################################
# Initial Read in of adsl dataset
################################################################################

adsl <- adsl_jnj

################################################################################
# Further script level parameters, after having read in main data
################################################################################

demog_vars <- c("REGION1", "COUNTRY", "SITEID")
## make it named vars so that demog_vars[xx] with xx subset of vars still works
names(demog_vars) <- demog_vars
## retrieve labels
demog_labels <- formatters::var_labels(adsl)[demog_vars]

### vars that have _decode version : use these instead of the original version
vars_decode <- paste0(demog_vars, "_DECODE")

demog_displ_vars <- tibble(orig = demog_vars, displ = vars_decode) %>%
  mutate(displ_exist = displ %in% names(adsl)) %>%
  mutate(finalvar = ifelse(displ_exist, displ, orig)) %>%
  pull(finalvar)

################################################################################
# Process data:
################################################################################

# filter and restrict to population of interest
adsl <- adsl %>%
  filter(.data[[popfl]] == "Y") %>%
  select(
    USUBJID,
    starts_with("TRT01"),
    all_of(popfls),
    all_of(unique(c(demog_vars, demog_displ_vars)))
  )


adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

# to ensure alphabetical ordering, as COUNTRY_DECODE is factor with order according COUNTRY, which is alphabetical on 3-letter code
adsl$REGION1 <- factor(
  as.character(adsl$REGION1),
  levels = sort(unique(as.character(adsl$REGION1)))
)
adsl$COUNTRY_DECODE <- factor(
  as.character(adsl$COUNTRY_DECODE),
  levels = sort(unique(as.character(adsl$COUNTRY_DECODE)))
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
) %>%
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) %>%
  split_cols_by(trtvar) %>%
  add_overall_col("Total") %>%
  split_rows_by(
    "REGION1",
    split_label = "Region",
    split_fun = trim_levels_in_group("COUNTRY_DECODE"),
    label_pos = "topleft",
    section_div = " "
  ) %>%
  summarize_row_groups("REGION1") %>%
  split_rows_by(
    "COUNTRY_DECODE",
    split_label = "Country/Territory",
    split_fun = trim_levels_in_group("SITEID"),
    label_pos = "topleft",
    section_div = " "
  ) %>%
  summarize_row_groups("COUNTRY_DECODE") %>%
  analyze_vars("SITEID", denom = "N_col", .stats = c("count_fraction")) %>%
  append_topleft("    Site, n (%)")

result <- build_table(lyt, adsl)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, titles)

################################################################################
# Convert to tbl file and output table:
################################################################################

tt_to_tlgrtf(result, file = fileid)
