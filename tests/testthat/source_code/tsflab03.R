#### BEFORE YOU START USING THIS PROGRAM ENSURE THE FOLLOWING: For your trial you should EITHER use lab toxicity grading (lbtoxgrade file) or Abnormality criteria (markedly abnormal file)
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

tblid <- "TSFLAB03"
fileid <- write_path(opath, tblid)
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

popfl <- "SAFFL"

trtvar <- "TRT01A"

ctrl_grp <- "Placebo"

combined_colspan_trt <- TRUE

if (combined_colspan_trt == TRUE) {
  add_combo <- add_combo_facet(
    "Combined",
    label = "Combined",
    levels = c("Xanomeline High Dose", "Xanomeline Low Dose")
  )

  rm_combo_from_placebo <- cond_rm_facets(
    facets = "Combined",
    ancestor_pos = NA,
    value = " ",
    split = "colspan_trt"
  )

  mysplit <- make_split_fun(post = list(add_combo, rm_combo_from_placebo))
}

## if the option TRTEMFL needs to be added to the TLF
trtemfl <- TRUE

## ANL flag variable for worst on-treatment low grade
anl_low_fl <- "ANL04FL"

## ANL flag variable for worst on-treatment high grade
anl_high_fl <- "ANL05FL"

## For analysis on SI units: use adlb dataset
## For analysis on Conventional units: use adlbc dataset -- shell is in conventional units

ad_domain <- "ADLB"

################################################################################
# Initial processing of data + check if table is valid for trial:
################################################################################
adlb_complete <- adlb_jnj

lbtoxgrade_file <- read_path(datapath, "lbtoxgrade.xlsx")

### CTC5 or DAIDS21c : default CTC5

lbtoxgrade_defs <- readxl::read_excel(lbtoxgrade_file, sheet = "CTC5")

lbtoxgrade_defs <- unique(
  lbtoxgrade_defs |>
    select(TOXTERM, TOXGRD, INDICATR)
) |>
  mutate(
    ATOXDSCLH = TOXTERM,
    ATOXGRLH = paste("Grade", TOXGRD)
  ) |>
  rename(ATOXDIR = INDICATR) |>
  select(ATOXDSCLH, ATOXGRLH, ATOXDIR)

################################################################################
# Process Data:
################################################################################

adsl <- adsl_jnj |>
  filter(.data[[popfl]] == "Y") |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c("Xanomeline Low Dose", "Xanomeline High Dose", "Placebo")
    )
  ) |>
  select(USUBJID, all_of(c(popfl, trtvar)))

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
adsl$rrisk_label <- paste(adsl[[trtvar]], paste("vs", ctrl_grp))

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

adlb00 <- adlb_complete |>
  select(
    USUBJID,
    AVISITN,
    AVISIT,
    starts_with("PAR"),
    starts_with("ATOX"),
    starts_with("ANL"),
    ONTRTFL,
    TRTEMFL,
    AVAL,
    APOBLFL,
    ABLFL,
    LVOTFL
  ) |>
  inner_join(adsl) |>
  mutate(
    ATOXGRL = as.character(ATOXGRL),
    ATOXGRH = as.character(ATOXGRH)
  ) |>
  relocate(
    USUBJID,
    all_of(anl_low_fl),
    all_of(anl_high_fl),
    ONTRTFL,
    TRTEMFL,
    AVISIT,
    ATOXGRL,
    ATOXGRH,
    ATOXDSCL,
    ATOXDSCH,
    PARAMCD,
    AVISIT,
    AVAL,
    APOBLFL,
    ABLFL
  )

# Filtered ADLB
filtered_adlb <- adlb00 |>
  filter(PARCAT4 == "Graded tests" & ONTRTFL == 'Y')

### low grades : ATOXDSCL ATOXGRL ANL04FL
### Note on Worst On-treatment
### note: by filter ANL04FL/ANL05FL, this table is restricted to On-treatment values, per definition of ANL04FL/ANL05FL
### therefor, no need to add ONTRTFL in filter
### if derivation of ANL04FL/ANL05FL is not restricted to ONTRTFL records, adding ONTRTFL here will not give the correct answer either
### as mixing worst with other period is not giving the proper selection !!!

filtered_adlb_low <- filtered_adlb |>
  filter(.data[[anl_low_fl]] == "Y" & !is.na(ATOXDSCL) & !is.na(ATOXGRL)) |>
  mutate(
    ATOXDSCLH = ATOXDSCL,
    ATOXGRLH = ATOXGRL,
    ATOXDIR = "LOW"
  ) |>
  select(USUBJID, starts_with("PAR"), starts_with("ATOX"), TRTEMFL) |>
  select(-c(ATOXGRL, ATOXGRH, ATOXDSCL, ATOXDSCH))

### high grades: ATOXDSCH ATOXGRH ANL05FL
filtered_adlb_high <- filtered_adlb |>
  filter(.data[[anl_high_fl]] == "Y" & !is.na(ATOXDSCH) & !is.na(ATOXGRH)) |>
  mutate(
    ATOXDSCLH = ATOXDSCH,
    ATOXGRLH = ATOXGRH,
    ATOXDIR = "HIGH"
  ) |>
  select(USUBJID, starts_with("PAR"), starts_with("ATOX"), TRTEMFL) |>
  select(-c(ATOXGRL, ATOXGRH, ATOXDSCL, ATOXDSCH))

## combine Low and high into adlb_tox
filtered_adlb_tox <-
  bind_rows(
    filtered_adlb_low,
    filtered_adlb_high
  ) |>
  select(-c(ATOXGR, ATOXGRN)) |>
  inner_join(adsl)

#### DO NOT USE TRTEMFL = Y in filter, as this will remove subjects from both numerator and denominator
#### instead : set ATOXGRLH to a non-reportable value (ie Grade 0) and keep in dataset
if (trtemfl) {
  filtered_adlb_tox <- filtered_adlb_tox |>
    mutate(
      ATOXGRLH = case_when(
        is.na(TRTEMFL) | TRTEMFL != "Y" ~ "0",
        TRUE ~ ATOXGRLH
      )
    )
}

## convert some to factors -- lty will fail if these are not factors
filtered_adlb_tox <-
  filtered_adlb_tox |>
  mutate(
    ATOXGRLH = factor(paste("Grade", ATOXGRLH), levels = paste("Grade", 0:5)),
    ATOXDIR = factor(ATOXDIR, levels = c("LOW", "HIGH"))
  )

filtered_adlb_tox <- unique(
  filtered_adlb_tox
)

check_non_unique_subject <- filtered_adlb_tox |>
  group_by(USUBJID, PARAMCD, ATOXDSCLH) |>
  summarize(n_subject = n()) |>
  filter(n_subject > 1)

if (nrow(check_non_unique_subject)) {
  message(
    "Please review your data selection process, subject has multiple records"
  )
}

### add relevant extra vars to lbtoxgrade_defs, only restrict to those actually in trial
lbtoxgrade_defs <- lbtoxgrade_defs |>
  inner_join(
    unique(
      filtered_adlb_tox |>
        select(PARAMCD, PARAM, ATOXDIR, ATOXDSCLH)
    ),
    relationship = "many-to-many"
  )

### Define param_map to be used in layout
param_map <- lbtoxgrade_defs |>
  select(PARAM, PARAMCD, ATOXDIR, ATOXDSCLH, ATOXGRLH) |>
  ### for proper sorting: add factor levels to PARAMCD, ATOXDIR
  mutate(ATOXDIR = factor(ATOXDIR, levels = c("LOW", "HIGH"))) |>
  ### actual sorting -- alphabetic by base term, LOW before HIGH within same term
  mutate(base_term = sub(", (low|high)$", "", as.character(ATOXDSCLH), ignore.case = TRUE)) |>
  arrange(base_term, ATOXDIR) |>
  select(-base_term) |>
  ### !!!! no factors are allowed in this split_fun map definition
  mutate(
    PARAMCD = as.character(PARAMCD),
    PARAM = as.character(PARAM),
    ATOXDIR = as.character(ATOXDIR),
    ATOXDSCLH = as.character(ATOXDSCLH),
    ATOXGRLH = as.character(ATOXGRLH)
  )

################################################################################
# Define layout and build table:
################################################################################
ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)

extra_args_rr <- list(
  method = "wald",
  denom = "n_df",
  ref_path = ref_path,
  .stats = c("denom", "count_unique_fraction")
)

lyt <- basic_table(show_colcounts = TRUE, colcount_format = "N=xx") |>
  ### first columns
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  )

if (combined_colspan_trt == TRUE) {
  lyt <- lyt |> split_cols_by(trtvar, split_fun = mysplit)
} else {
  lyt <- lyt |> split_cols_by(trtvar)
}

lyt <- lyt |>
  split_cols_by("rrisk_header", nested = FALSE) |>
  split_cols_by(
    trtvar,
    labels_var = "rrisk_label",
    split_fun = remove_split_levels(ctrl_grp)
  ) |>
  split_rows_by(
    "ATOXDSCLH",
    label_pos = "topleft",
    child_labels = "visible",
    split_label = "Laboratory Test",
    ### trim_levels_to_map needs to be applied at ALL split_rows_by levels
    split_fun = trim_levels_to_map(param_map),
    section_div = " "
  ) |>
  append_topleft("    Grade, n (%)") |>
  analyze(
    "ATOXGRLH",
    a_freq_j,
    extra_args = extra_args_rr,
    show_labels = "hidden",
    indent_mod = 0L
  )

result <- build_table(lyt, filtered_adlb_tox, alt_counts_df = adsl, round_type = 'sas')

################################################################################
# Post-Processing:
# - Remove Grade 0 line
# - Remove colcount from rrisk_header
################################################################################

remove_grade0 <- function(tr) {
  if (is(tr, "DataRow") & (tr@label == "Grade 0")) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

result <- result |> prune_table(prune_func = keep_rows(remove_grade0))

################################################################################
# Remove colcount from rrisk_header:
################################################################################

result <- remove_col_count(result)

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table
################################################################################


colwidth <- c(54, 19, 21, 21, 21, 35, 33)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "landscape")
