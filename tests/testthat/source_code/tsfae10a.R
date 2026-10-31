library(envsetup)
library(tern)
library(forcats)
library(dplyr)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSFAE10a"
titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01A"
subjFilterText <- "AE"
ctrl_grp <- "Placebo"

################################################################################
# Process Data
# - Sub-setting performed on a subject-level; certain tables may require a more
#   granular, column-based sub-setting.
# - Factor for Active Treatment spanning header added below.
# - Additional factor reformatting added below.
################################################################################

adsl <- adsl_jnj |>
  filter(!!rlang::sym(popfl) == "Y") |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c(
        "Xanomeline Low Dose",
        "Xanomeline High Dose",
        "Placebo"
      )
    ),
    # Canonical demographics decoding (replace * usage)
    SEX = factor(
      dplyr::case_when(
        SEX == "F" ~ "Female",
        SEX == "M" ~ "Male",
        SEX == "U" ~ "Unknown",
        SEX == "INTERSEX" ~ "Intersex"
      ),
      levels = c("Male", "Female", "Intersex", "Unknown")
    ),
    AGEGR1 = factor(
      AGEGR1,
      levels = c(">=18 to <65", ">=65 to <75", ">=75")
    ),
    RACE = factor(
      dplyr::case_when(
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
      dplyr::case_when(
        ETHNIC == "HISPANIC OR LATINO" ~ "Hispanic or Latino",
        ETHNIC == "NOT HISPANIC OR LATINO" ~ "Not Hispanic or Latino",
        ETHNIC == "NOT REPORTED" ~ "Not reported",
        ETHNIC == "UNKNOWN" ~ "Unknown"
      ),
      levels = c("Hispanic or Latino", "Not Hispanic or Latino", "Not reported", "Unknown")
    )
  ) |>
  create_colspan_var(
    non_active_grp = ctrl_grp,
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = trtvar
  ) |>
  select(
    USUBJID,
    !!rlang::sym(popfl),
    !!rlang::sym(trtvar),
    SEX,
    AGEGR1,
    RACE,
    ETHNIC,
    colspan_trt
  )


# Factor reformatting (e.g., Include missing in the "Unknown" category).
adsl$SEX <- forcats::fct_na_value_to_level(
  adsl$SEX,
  level = "Unknown"
)

adsl$AGEGR1 <- forcats::fct_na_value_to_level(
  adsl$AGEGR1,
  level = "Unknown"
)

adsl$RACE <- forcats::fct_collapse(
  forcats::fct_na_value_to_level(adsl$RACE, level = "Unknown"),
  "Not reported or unknown" = c("Not reported", "Unknown")
)

adsl$ETHNIC <- forcats::fct_collapse(
  forcats::fct_na_value_to_level(adsl$ETHNIC, level = "Unknown"),
  "Not reported or unknown" = c("Not reported", "Unknown")
)

had_ae <- adae_jnj |>
  filter(TRTEMFL == "Y") |>
  select(USUBJID, TRTEMFL) |>
  distinct(USUBJID, .keep_all = TRUE)

adsl <- adsl |>
  left_join(had_ae) |>
  mutate(TRTEMFL = ifelse(is.na(TRTEMFL), "N", "Y"))

################################################################################
# Define layout and build table
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

add_active_combo <- make_split_fun(
  post = list(
    add_combo_facet(
      name = "Combined",
      label = "Combined",
      levels = c("Xanomeline High Dose", "Xanomeline Low Dose")
    ),
    cond_rm_facets(
      facets = "Combined",
      ancestor_pos = NA,
      value = " ",
      split = "colspan_trt"
    )
  )
)

extra_args_rr <- list(
  riskdiff = FALSE
)

extra_args_rr2 <- append(
  extra_args_rr,
  list(resp_var = "TRTEMFL", drop_levels = TRUE)
)

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx",
  top_level_section_div = " "
) |>
  append_topleft("Characteristic") |>
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) |>
  split_cols_by(trtvar, split_fun = add_active_combo)

lyt <- lyt |>
  analyze(
    "TRTEMFL",
    afun = a_freq_j,
    extra_args = append(
      extra_args_rr,
      list(
        label = paste("Subjects with >= 1", subjFilterText),
        val = "Y",
        .stats = c("count_unique_fraction")
      )
    ),
    show_labels = "hidden"
  ) |>
  analyze(
    vars = "SEX",
    var_labels = "Sex, n/Ns (%)",
    show_labels = "visible",
    afun = a_freq_resp_var_j,
    extra_args = extra_args_rr2,
    nested = FALSE
  ) |>
  analyze(
    vars = "AGEGR1",
    var_labels = "Age group (years), n/Ns (%)",
    show_labels = "visible",
    afun = a_freq_resp_var_j,
    extra_args = extra_args_rr2,
    nested = FALSE
  ) |>
  analyze(
    vars = "RACE",
    var_labels = "Race, n/Ns (%)",
    show_labels = "visible",
    afun = a_freq_resp_var_j,
    extra_args = extra_args_rr2,
    nested = FALSE
  ) |>
  analyze(
    vars = "ETHNIC",
    var_labels = "Ethnicity, n/Ns (%)",
    show_labels = "visible",
    afun = a_freq_resp_var_j,
    extra_args = extra_args_rr2,
    nested = FALSE
  )

result <- build_table(lyt, adsl, round_type = "sas")

################################################################################
# Post-Processing:
# Prune any categories with all zeros:
################################################################################

result <- safe_prune_table(result, prune_func = count_pruner())

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, titles)

################################################################################
# Convert to tbl file and output table
################################################################################


colwidth <- c(64, 27, 27, 29, 27)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "landscape")
