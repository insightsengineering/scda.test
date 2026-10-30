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

# Flag to indicate whether IQR should be presented in the table
add_interquartile_range <- FALSE

.stats_tbl <- c(
  "n",
  "mean_sd",
  "median",
  "range",
  "count_fraction",
  if (add_interquartile_range) "quantiles" else NULL
)

################################################################################
# Initial Read in of adsl dataset
################################################################################

adsl <- adsl_jnj |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c("Xanomeline Low Dose", "Xanomeline High Dose", "Placebo")
    ),
    SEX = factor(
      case_when(SEX == "M" ~ "Male", SEX == "F" ~ "Female", TRUE ~ SEX),
      levels = c("Male", "Female", "Intersex", "Unknown")
    ),
    COUNTRY = factor(
      case_when(COUNTRY == "USA" ~ "United States", TRUE ~ NA_character_)
    ),
    RACE = factor(
      case_when(
        RACE == "AMERICAN INDIAN OR ALASKA NATIVE" ~ "American Indian or Alaska Native",
        RACE == "ASIAN" ~ "Asian",
        RACE == "BLACK OR AFRICAN AMERICAN" ~ "Black or African American",
        RACE == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ "Native Hawaiian or other Pacific Islander",
        RACE == "WHITE" ~ "White",
        RACE == "MULTIPLE" ~ "Multiple",
        RACE == "NOT REPORTED" ~ "Not reported",
        RACE == "UNKNOWN" ~ "Unknown",
        RACE == "OTHER" ~ "Other"
      ),
      levels = c(
        "American Indian or Alaska Native",
        "Asian",
        "Black or African American",
        "Native Hawaiian or other Pacific Islander",
        "White",
        "Multiple",
        "Not reported",
        "Unknown",
        "Other"
      )
    ),
    ETHNIC = factor(
      case_when(
        ETHNIC == "HISPANIC OR LATINO" ~ "Hispanic or Latino",
        ETHNIC == "NOT HISPANIC OR LATINO" ~ "Not Hispanic or Latino",
        ETHNIC == "NOT REPORTED" ~ "Not reported",
        ETHNIC == "UNKNOWN" ~ "Unknown"
      ),
      levels = c(
        "Hispanic or Latino",
        "Not Hispanic or Latino",
        "Not reported",
        "Unknown"
      )
    ),
    BMIBLG1 = factor(
      .data[["BMIBLG1"]],
      levels = c("Underweight <18.5", "Normal >=18.5 to <25", "Overweight >=25 to <30", "Obese >=30")
    ),
    WGTGR1 = factor(.data[["WGTGR1"]], levels = c("<30", ">=30 to <60", ">=60 to <90", ">=90"))
  ) |>
  labelled::set_variable_labels(
    COUNTRY = "Country/Territory",
    SEX = "Sex",
    AGE = "Age (years)",
    RACE = "Race",
    ETHNIC = "Ethnicity",
    BMIBLG1 = "BMI at Baseline Group 1",
    WGTGR1 = "Weight Group 1",
    REGION1 = "Region",
    BSABL = paste0("Body surface area (m~[super 2])"),
    BMIBL = paste0("Body mass index (kg/m~[super 2])")
  )

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

## to assign precision and post processing
demog_displ_vars <- demog_vars
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

## JJCS standards: split >= 65 into 2 levels
new_age_levels <- list(c(">=65"), list(c(">=65 to <75", ">=75")))

### NOTE: For AGEGR1 ", n(%)" will be added to these levels by the custom analysis function a_freq_j

### For BMIBLG1 :add ", n(%)" to the levels of the variable -- not ideal, but the easiest way to get it done

levelsBMI <- levels(adsl$BMIBLG1)
adsl$BMIBLG1 <- factor(
  as.character(adsl$BMIBLG1),
  levels = levelsBMI,
  labels = paste0(levelsBMI, ", n (%)")
)

### For WGTGR1 :add ", n(%)" to the levels of the variable -- not ideal, but the easiest way to get it done

levelsWGT <- levels(adsl$WGTGR1)
adsl$WGTGR1 <- factor(
  as.character(adsl$WGTGR1),
  levels = levelsWGT,
  labels = paste0(levelsWGT, ", n (%)")
)

# to ensure alphabetical ordering, as COUNTRY_DECODE is factor with order according COUNTRY, which is alphabetical on 3-letter code
adsl$COUNTRY <- factor(
  as.character(adsl$COUNTRY),
  levels = sort(unique(as.character(adsl$COUNTRY)))
)

################################################################################
# Process data:
################################################################################

## restrict to core variables only and restrict to population
adsl <- adsl |>
  select(
    USUBJID,
    starts_with("TRT01"),
    all_of(c(demog_vars, popfl, "AGEGR1N"))
  ) |>
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
  ) |>
    pull(decimal)

  cat(paste("Precision of variable", var, ":", prec, "\n"))

  return(prec)
}

# list here all numerical formats with their d - either manually or via prec_var,

var_d <- c("AGE" = prec_var("AGE"), "WEIGHTBL" = 1, "HEIGHTBL" = 1, "BMIBL" = 1, "BSABL" = 1)

# need formats for all variables, including categorical ones
demog_vars_d <- rep(0, length(demog_vars))
names(demog_vars_d) <- names(demog_vars)
### set categorical ones to proper d
demog_vars_d[names(var_d)] <- var_d

fmt_d <- fmt_spec_var_d(demog_vars_d, stats_in = .stats_tbl, fmt_d_def = junco_def_d_all, fmt_d_in = NULL)

### AGEGR1 needs special attention - as need an extra combined age level
pos_AGEGR1 <- which(demog_displ_vars == "AGEGR1")

if (identical(pos_AGEGR1, integer(0))) {
  P1 <- 1:length(demog_displ_vars)
} else {
  P1 <- 1:(pos_AGEGR1 - 1)
}

P2 <- (pos_AGEGR1 + 1):length(demog_displ_vars)
### If AGEGR1 is the last var to be displayed, P2 can be ignored

################################################################################
# Define layout and build table:
################################################################################

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) |>
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) |>
  split_cols_by(trtvar) |>
  add_overall_col("Total") |>
  append_topleft("Characteristic") |>
  ### analyze vars prior to AGEGR1
  analyze(
    vars = demog_displ_vars[P1],
    var_labels = demog_labels[P1],
    afun = a_summary,
    extra_args = list(
      .stats = .stats_tbl,
      .labels = c("n" = "N", "range" = "Min, max", "quantiles" = "Interquartile range"),
      .formats = "default",
      .indent_mods = c(
        "n" = 0L,
        "mean_sd" = 1L,
        "median" = 1L,
        "range" = 1L,
        "count_fraction" = 1L,
        "quantiles" = 1L
      )
    ),
    format = fmt_d
    # ,section_div = " "
  ) |>
  ### special requirements for AGEGR1 : add extra combined level
  analyze(
    vars = "AGEGR1",
    afun = a_freq_j,
    extra_args = list(
      denom = "n_df",
      new_levels = new_age_levels,
      .indent_mods = 2L,
      addstr2levs = ", n (%)",
      .stats = c("count_unique_fraction")
    )
  ) |>
  ### continue with the remainder vars (if AGEGR1 is not the last variable)
  analyze(
    vars = demog_displ_vars[P2],
    var_labels = demog_labels[P2],
    afun = a_summary,
    extra_args = list(
      .stats = .stats_tbl,
      .labels = c("n" = "N", "range" = "Min, max", "quantiles" = "Interquartile range"),
      .formats = "default",
      .indent_mods = c(
        "n" = 0L,
        "mean_sd" = 1L,
        "median" = 1L,
        "range" = 1L,
        "count_fraction" = 1L,
        "quantiles" = 1L
      )
    ),
    format = fmt_d,
    # ,section_div = " "
  )

result <- build_table(lyt, adsl, round_type = "sas")

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
tt_at_path(result, path = c("BMIBLG1", "n")) <- NULL

tt_at_path(result, path = c("WGTGR1", "n")) <- NULL

label_at_path(result, path = c("AGEGR1")) <- NULL
label_at_path(result, path = c("BMIBLG1")) <- NULL
label_at_path(result, path = c("WGTGR1")) <- NULL

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
  var = "BMIBLG1",
  levels = levels(adsl$BMIBLG1),
  addindent = rep(1, times = length(levels(adsl$BMIBLG1)))
)
result <- upd_indent_mod(
  result,
  var = "WGTGR1",
  levels = levels(adsl$WGTGR1),
  addindent = rep(1, times = length(levels(adsl$WGTGR1)))
)

result <- result |>
  prune_table(
    prune_func = prune_empty_level
  )
################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, titles)

################################################################################
# Convert to tbl file and output table:
################################################################################


colwidth <- c(64, 27, 27, 27, 27)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid)
