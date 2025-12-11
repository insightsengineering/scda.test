################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsfecg04
## R version:                 4.2.1
## Short Description:         Program to create tsfecg04: Shift From Baseline to
##                            Maximum On-treatment Corrected QT Interval
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      30JAN2024
## Input:                     adsl.RDS, adeg.RDS
## Output:                    tsfecg04.rtf
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
# Prep Environment
################################################################################

library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)
library(rlang)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSFECG04"
fileid <- write_path(opath, tblid)
titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

popfl <- "SAFFL"
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"
demogvars <- c("SEX", "AGEGR1", "RACE", "ETHNIC", "AGE")

ad_domain <- "ADEG"

## selection of QTC parameters
selparamcd <- c("QTCFAG", "QTCBAG", "QTCS", "QTCLAG")

################################################################################
# initial read of data
################################################################################
adeg_complete <- adeg_jnj

### available QTC parameters in study
selparamcd <- intersect(selparamcd, unique(adeg_complete$PARAMCD))

catvar <- "AVALCAT1"

################################################################################
# Process Data:
################################################################################

adsl <- adsl_jnj %>%
  filter(!!sym(popfl) == "Y") %>%
  select(STUDYID, USUBJID, all_of(c(trtvar, popfl, demogvars)))

adsl <- adsl %>%
  mutate(
    colspan_trt = factor(
      ifelse(!!sym(trtvar) == ctrl_grp, " ", "Active Study Agent"),
      levels = c("Active Study Agent", " ")
    )
  )
## to ensure the same order as on other outputs
trt_order <- as.character((unique(
  adsl %>% select("colspan_trt", all_of(trtvar))
) %>%
  arrange(colspan_trt, !!sym(trtvar)))[[trtvar]])
adsl[[trtvar]] <- factor(as.character(adsl[[trtvar]]), levels = trt_order)

adeg <- adeg_complete %>%
  filter(PARAMCD %in% selparamcd) %>%
  ### Maximum On-treatment
  ### note: by filter ANL03FL, this table is restricted to On-treatment values, per definition of ANL03FL
  ### therefor, no need to add ONTRTFL in filter
  ### if derivation of ANL03FL is not restricted to ONTRTFL records, adding ONTRTFL here will not give the correct answer either
  ### as mixing worst with other period is not giving the proper selection !!!
  filter(ANL03FL == "Y") %>%
  select(
    USUBJID,
    ONTRTFL,
    TRTEMFL,
    PARAM,
    PARAMCD,
    AVISITN,
    AVISIT,
    AVAL,
    BASE,
    CHG,
    AVALCAT1,
    BASECAT1,
    ONTRTFL,
    TRTEMFL,
    ANL01FL,
    ANL02FL,
    ANL03FL
  ) %>%
  inner_join(., adsl)

adeg$AVISIT <- factor("Maximum Corrected QT Interval")

## if also over time is needed, append these to above dataset
## see ecg05 as example

check1 <- adeg %>%
  group_by(TRT01A, PARAMCD, AVISIT) %>%
  summarize(n = n_distinct(USUBJID))


## add variable for column split header
adeg$BASECAT1_header <- "Baseline Corrected QT Interval"
adeg$BASECAT1_header2 <- " " ## first column N should not appear under Baseline column span

adeg$BASECAT1_header3 <- " " ## extra to allow for additional topleft material

###
AVALCAT1_levels <- levels(adeg$AVALCAT1)


## add extra level N to Basecat1
adeg <- adeg %>%
  mutate(
    BASECAT1 = factor(as.character(BASECAT1), levels = c("N", AVALCAT1_levels))
  )


## trick for alt_counts_df to work with col splitting
# add BASECAT1 to adsl, all assign to extra level N (column will be used for N counts)
adslx <- adsl %>%
  mutate(BASECAT1 = "N") %>%
  mutate(BASECAT1 = factor(BASECAT1, levels = c("N", AVALCAT1_levels)))


adslx$BASECAT1_header <- "Baseline Corrected QT Interval"
adslx$BASECAT1_header2 <- " "
adslx$BASECAT1_header3 <- " " ## extra to allow for additional topleft material

################################################################################
# Define layout and build table:
################################################################################

lyt <- basic_table(show_colcounts = FALSE) %>%
  split_cols_by("BASECAT1_header3") %>%
  ## to ensure N column is not under the Baseline column span header
  split_cols_by("BASECAT1_header2") %>%
  split_cols_by("BASECAT1", split_fun = keep_split_levels("N")) %>%
  ## restart column split (Nested = False)
  ## Combined levels will be made, and the N column should not appear
  split_cols_by("BASECAT1_header", nested = FALSE) %>%
  split_cols_by(
    "BASECAT1",
    split_fun = make_split_fun(
      pre = list(rm_levels(excl = "N")),
      post = list(
        add_overall_facet("TOTAL", "Total")
      )
    )
  ) %>%
  #### replace split_rows and summarize by single analyze call
  ### a_freq_j only works due to
  ### special arguments can do the trick : denomf = adslx & .stats = count_unique
  ### we want counts of treatment group coming from adsl, not from input dataset, therefor, countsource = altdf
  analyze(
    vars = trtvar,
    afun = a_freq_j,
    extra_args = list(
      restr_columns = "N",
      .stats = "count_unique",
      countsource = "altdf",
      extrablankline = TRUE
    ),
    indent_mod = -1L
  ) %>%
  # ## main part of table
  split_rows_by(
    "PARAM",
    nested = FALSE,
    label_pos = "topleft",
    child_labels = "visible",
    split_label = "QTc Interval",
    split_fun = drop_split_levels,
    section_div = " "
  ) %>%
  split_rows_by(
    trtvar,
    label_pos = "topleft",
    indent_mod = -1L,
    child_labels = "hidden",
    split_label = "Treatment Group",
    section_div = " "
  ) %>%
  ### a_freq_j
  ### the special statistic "n_rowdf" option does the trick here of getting the proper value for the N column
  summarize_row_groups(
    trtvar,
    cfun = a_freq_j,
    extra_args = list(
      .stats = "n_rowdf",
      restr_columns = c("N")
    )
  ) %>%
  split_rows_by(
    "AVISIT",
    label_pos = "hidden",
    indent_mod = 1L,
    split_label = " ",
    child_labels = "visible",
    section_div = " "
  ) %>%
  ## add extra level TOTAL using new_levels, rather than earlier technique
  ## advantage for denominator derivation -- n_rowdf can be used, if we'd like to present fraction as well
  ## switch .stats to count_unique_denom_fraction or count_unique_fraction
  analyze(
    "AVALCAT1",
    afun = a_freq_j,
    extra_args = list(
      .stats = "count_unique",
      denom = "n_rowdf",
      new_levels = list(c("Total"), list(AVALCAT1_levels)),
      new_levels_after = TRUE,
      .indent_mods = 1L,
      restr_columns = c(
        toupper(AVALCAT1_levels),
        "TOTAL"
      )
    )
  ) %>%
  append_topleft("    Criteria, n")

result <- build_table(lyt, adeg, alt_counts_df = adslx)
# head(result, 20)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, titles)

################################################################################
# Convert to tbl file and output table
################################################################################

# the default column-widths had issues with 2 columns appear too close
# retrieve the default column widths and update the latter columns
fontspec <- font_spec("Times", 9L, 1.2)
col_gap <- 7L
label_width_ins <- 2

colwidths <- def_colwidths(
  result,
  fontspec,
  col_gap = col_gap,
  label_width_ins = label_width_ins
)

# adjust the column-widths to have the same length for columns 3 - 7 (<= 450, ...., Total)
acolwidths <- colwidths
acolwidths[3:length(acolwidths)] <- 12

tt_to_tlgrtf(result, file = fileid, colwidths = acolwidths)
