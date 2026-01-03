################################################################################
## Original Reporting Effort: Standards
## Program Name:              tsidem01
## R version:                 4.2.1
## Short Description:         Program to create tsidem01: Demographics and Baseline Characteristics
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      30JAN2024
## Input:                     adsl.RDS
## Output:                    tsidem01.rtf
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

tblid <- "TSIDEM01"
fileid <- write_path(opath, tblid)
titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

popfl <- "FASFL"
trtvar <- "TRT01P"
ctrl_grp <- "Placebo"

################################################################################
# Initial Read in of adsl dataset
################################################################################

adsl <- adsl_jnj %>%
  labelled::set_variable_labels(COUNTRY = "Country/Territory")

################################################################################
# Further script level parameters, after having read in main data
################################################################################

demog_vars <- c(
  "SEX",
  "AGE",
  "AGEGR1",
  "RACE",
  "ETHNIC",
  "WEIGHTBL",
  "WGTGR1",
  "HEIGHTBL",
  "BMIBL",
  "BMIBLG1",
  "BSABL",
  "REGION1",
  "COUNTRY"
)
## make it named vars so that demog_vars[xx] with xx subset of vars still works
names(demog_vars) <- demog_vars
## retrieve labels
demog_labels <- formatters::var_labels(adsl)[demog_vars]

cat_vars <- c(
  "SEX",
  "AGEGR1",
  "RACE",
  "ETHNIC",
  "WGTGR1",
  "BMIBLG1",
  "REGION1",
  "COUNTRY"
)
cat_vars <- intersect(cat_vars, demog_vars)
# categorical vars get ", n (%)" added into the label
demog_labels[cat_vars] <- paste0(demog_labels[cat_vars], ", n (%)")

### vars that have _decode version : use these instead of the original version
vars_decode <- paste0(demog_vars, "_DECODE")

demog_displ_vars <- tibble(orig = demog_vars, displ = vars_decode) %>%
  mutate(displ_exist = displ %in% names(adsl)) %>%
  mutate(finalvar = ifelse(displ_exist, displ, orig)) %>%
  pull(finalvar)


BMIBLG1_avar <- intersect(demog_displ_vars, c("BMIBLG1", "BMIBLG1_DECODE"))
WGTGR1_avar <- intersect(demog_displ_vars, c("WGTGR1", "WGTGR1_DECODE"))
AGEGR1_avar <- intersect(demog_displ_vars, c("AGEGR1", "AGEGR1_DECODE"))

## JJCS standards: split >= 65 into 2 levels
new_age_levels <- list(c(">=65"), list(c(">=65 to <75", ">=75")))

### NOTE: For AGEGR1 ", n(%)" will be added to these levels by the custom analysis function a_freq_j

### For BMIBLG1 :add ", n(%)" to the levels of the variable -- not ideal, but the easiest way to get it done

levelsBMI <- levels(adsl[[BMIBLG1_avar]])
adsl[[BMIBLG1_avar]] <- factor(
  as.character(adsl[[BMIBLG1_avar]]),
  levels = levelsBMI,
  labels = paste0(levelsBMI, ", n (%)")
)

### For WGTGR1 :add ", n(%)" to the levels of the variable -- not ideal, but the easiest way to get it done
levelsWGT <- levels(adsl[[WGTGR1_avar]])
adsl[[WGTGR1_avar]] <- factor(
  as.character(adsl[[WGTGR1_avar]]),
  levels = levelsWGT,
  labels = paste0(levelsWGT, ", n (%)")
)

# to ensure alphabetical ordering, as COUNTRY_DECODE is factor with order according COUNTRY, which is alphabetical on 3-letter code
adsl$COUNTRY_DECODE <- factor(
  as.character(adsl$COUNTRY_DECODE),
  levels = sort(unique(as.character(adsl$COUNTRY_DECODE)))
)

################################################################################
# Process data:
################################################################################

## restrict to core variables only and restrict to population
adsl <- adsl %>%
  select(
    USUBJID,
    starts_with("TRT01"),
    all_of(c(demog_vars, demog_displ_vars, popfl, "AGEGR1N"))
  ) %>%
  filter(.data[[popfl]] == "Y")


adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

prec_var <- function(var, cap = 4) {
  prec <- tidytlg:::make_precision_data(
    df = adsl,
    decimal = cap,
    precisionby = NULL,
    precisionon = var
  ) %>%
    pull(decimal)

  cat(paste("Precision of variable", var, ":", prec, "\n"))

  return(prec)
}

prec_age <- prec_var("AGE")
prec_weightbl <- prec_var("WEIGHTBL")
prec_heightbl <- prec_var("HEIGHTBL")
prec_bmibl <- prec_var("BMIBL")
prec_bsabl <- prec_var("BSABL")

# precision is set manually, the above is just for checking
# note that current precision has been capped to decimal 1 in the below (even for the 2 parameters with higher precision in the database: BMIBL and BSABL)

### AGEGR1 needs special attention
pos_AGEGR1 <- which(demog_displ_vars == AGEGR1_avar)

if (identical(pos_AGEGR1, integer(0))) {
  P1 <- 1:length(demog_displ_vars)
} else {
  P1 <- 1:(pos_AGEGR1 - 1)
}

# set numerical precision : only AGE is in P1 : precision d = 0
P1_precision <- jjcs_num_formats(d = 0)$fmt


P2 <- (pos_AGEGR1 + 1):length(demog_displ_vars)
### If AGEGR1 is the last var to be displayed, P2 can be ignored

### further split P2 in calls: different precision is needed for WEIGHTBL, HEIGHTBL (input data as 1 digit) and BMIBL, BSA (input data as 2 digits)
pos_BMIBL <- which(demog_displ_vars == "BMIBL")
P2a <- P2[P2 < pos_BMIBL]
P2b <- P2[P2 >= pos_BMIBL]

# set numerical precision P2a:  WEIGHTBL, HEIGHTBL : precision d = 1
P2a_precision <- jjcs_num_formats(d = 1)$fmt
# Per communication of Joyce: leave BMI and BSA to precision 1 as well
# set numerical precision P2b:  BMIBL, BSA : precision d = 1
### hence, the splitting up of P2 into P2a and P2b is not really needed, but kept to demonstrate how this could be achieved
P2b_precision <- jjcs_num_formats(d = 1)$fmt

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
  append_topleft("Characteristic") %>%
  ### analyze vars prior to AGEGR1
  analyze(
    vars = demog_displ_vars[P1],
    var_labels = demog_labels[P1],
    afun = a_summary,
    extra_args = list(
      .stats = c("n", "mean_sd", "median", "range", "count_fraction"),
      .labels = c("n" = "N", "range" = "Min, max"),
      .formats = c(P1_precision, "count_fraction" = jjcsformat_count_fraction),
      .indent_mods = c(
        "n" = 0L,
        "mean_sd" = 1L,
        "median" = 1L,
        "range" = 1L,
        "count_fraction" = 1L
      )
    )
    # ,section_div = " "
  ) %>%
  ### special requirements for AGEGR1 : add extra combined level
  analyze(
    vars = AGEGR1_avar,
    afun = a_freq_j,
    extra_args = list(
      denom = "n_df",
      new_levels = new_age_levels,
      .indent_mods = 2L,
      addstr2levs = ", n (%)",
      .stats = c("count_unique_fraction")
    )
  ) %>%
  ### continue with the remainder vars (if AGEGR1 is not the last variable)
  analyze(
    vars = demog_displ_vars[P2a],
    var_labels = demog_labels[P2a],
    afun = a_summary,
    extra_args = list(
      .stats = c("n", "mean_sd", "median", "range", "count_fraction"),
      .labels = c("n" = "N", "range" = "Min, max"),
      .formats = c(P2a_precision, "count_fraction" = jjcsformat_count_fraction),
      .indent_mods = c(
        "n" = 0L,
        "mean_sd" = 1L,
        "median" = 1L,
        "range" = 1L,
        "count_fraction" = 1L
      )
    )
    # ,section_div = " "
  ) %>%
  analyze(
    vars = demog_displ_vars[P2b],
    var_labels = demog_labels[P2b],
    afun = a_summary,
    extra_args = list(
      .stats = c("n", "mean_sd", "median", "range", "count_fraction"),
      .labels = c("n" = "N", "range" = "Min, max"),
      .formats = c(P2b_precision, "count_fraction" = jjcsformat_count_fraction),
      .indent_mods = c(
        "n" = 0L,
        "mean_sd" = 1L,
        "median" = 1L,
        "range" = 1L,
        "count_fraction" = 1L
      )
    )
    # ,section_div = " "
  )

result <- build_table(lyt, adsl)

################################################################################
# Post-Processing:
# - update section dividers -- adds in blank line at appropriate place
# - remove N and label for BMI, AGEGR1, WGTGR1
# - remove section dividers after AGE, BMIBL, WGTGR1
# - update indents
################################################################################

# update section dividers
section_div(result, only_sep_sections = TRUE) <- " "

# remove N and label for BMI, AGEGR1, WGTGR1 (only label)
tt_at_path(result, path = c(BMIBLG1_avar, "n")) <- NULL

tt_at_path(result, path = c(WGTGR1_avar, "n")) <- NULL

label_at_path(result, path = c(AGEGR1_avar)) <- NULL
label_at_path(result, path = c(BMIBLG1_avar)) <- NULL
label_at_path(result, path = c(WGTGR1_avar)) <- NULL

# Remove some section dividers : after AGE, BMIBL, WEIGHTBL
rpths <- row_paths(result)

# identify list with label
gettbl_label_p1 <- function(label) {
  function(x) {
    z <- which(x == label)
    z <- !identical(z, integer(0))
    return(z)
  }
}

get_trpath_label <- function(rpths, label) {
  mypth <- rpths[[min(which(unlist(lapply(
    rpths,
    FUN = gettbl_label_p1(label)
  ))))]]
}

section_div_at_path(result, get_trpath_label(rpths, "BMIBL")) <- NA_character_
section_div_at_path(result, get_trpath_label(rpths, "AGE")) <- NA_character_
section_div_at_path(
  result,
  get_trpath_label(rpths, "WEIGHTBL")
) <- NA_character_


### update indents

upd_indent_mod <- function(result, var, levels, addindent) {
  for (i in 1:length(levels)) {
    addindi <- addindent[i]
    leveli <- paste0("count_fraction.", levels[i])
    path <- c(var, leveli)
    indent_mod(tt_at_path(result, path)) <- indent_mod(tt_at_path(
      result,
      path
    )) +
      addindi
  }
  return(result)
}

result <- upd_indent_mod(
  result,
  var = BMIBLG1_avar,
  levels = levels(adsl[[BMIBLG1_avar]]),
  addindent = rep(1, times = length(levels(adsl[[BMIBLG1_avar]])))
)
result <- upd_indent_mod(
  result,
  var = WGTGR1_avar,
  levels = levels(adsl[[WGTGR1_avar]]),
  addindent = rep(1, times = length(levels(adsl[[WGTGR1_avar]])))
)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, titles)

################################################################################
# Convert to tbl file and output table:
################################################################################

tt_to_tlgrtf(result, file = fileid)
