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

tblid <- "TSFLAB07"
fileid <- write_path(opath, tblid)
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

popfl <- "SAFFL"

# Actual treatment variable (default=TRT01A).
trtvar <- "TRT01A"

ctrl_grp <- "Placebo"

ad_domain <- "ADLB"

# Flag variable for worst low toxicity grade (default=ANL04FL).
anl_low_fl <- "ANL04FL"
# Flag variable for worst high toxicity grade (default=ANL05FL).
anl_high_fl <- "ANL05FL"

# Show percentage alongside count (default=FALSE).
show_pct <- FALSE

################################################################################
# Initial processing of data
################################################################################

adlb_complete <- adlb_jnj

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
  select(STUDYID, USUBJID, all_of(c(popfl, trtvar)))

flagvars <- c("ONTRTFL", "TRTEMFL", "LVOTFL", "ABLFL")
adlb00 <- adlb_complete |>
  filter(!is.na(ATOXGR)) |>
  filter(!is.na(BTOXGR)) |>
  select(
    USUBJID,
    AVISITN,
    AVISIT,
    PARAMCD,
    PARAM,
    PARAMN,
    PARCAT1,
    PARCAT3,
    PARCAT4,
    PARCAT5,
    PARCAT6,
    starts_with("ATOX"),
    starts_with("BTOX"),
    starts_with("ANL"),
    all_of(flagvars),
    LBSEQ,
    AVAL,
    AVALC
  ) |>
  inner_join(adsl) |>
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
    ABLFL,
    BTOXGRL,
    BTOXGRH,
    PARAMCD,
    PARCAT3,
    AVISIT,
    AVAL
  )

adlb00 <- var_relabel_list(adlb00, var_labels(adlb_complete, fill = T))

filtered_adlb <- adlb00

## similar to LAB03: separate low and high and then combine
## esp as for 2 tests PARCAT5/6 is not the same for low/high direction

filtered_adlb_low <- filtered_adlb |>
  filter(!is.na(ATOXDSCL) & !is.na(ATOXGRL)) |>
  mutate(
    ATOXDSCLH = ATOXDSCL,
    ATOXGRLH = ATOXGRL,
    BTOXGRLH = BTOXGRL,
    ATOXDIR = "LOW",
    ANLHLFL = .data[[anl_low_fl]]
  ) |>
  select(-c(ATOXGRL, ATOXGRH, ATOXDSCL, ATOXDSCH, BTOXGRL, BTOXGRH))

### high grades: ATOXDSCH ATOXGRH ANL05FL
filtered_adlb_high <- filtered_adlb |>
  filter(!is.na(ATOXDSCH) & !is.na(ATOXGRH)) |>
  mutate(
    ATOXDSCLH = ATOXDSCH,
    ATOXGRLH = ATOXGRH,
    BTOXGRLH = BTOXGRH,
    ATOXDIR = "HIGH",
    ANLHLFL = .data[[anl_high_fl]]
  ) |>
  select(-c(ATOXGRL, ATOXGRH, ATOXDSCL, ATOXDSCH, BTOXGRL, BTOXGRH))


## combine Low and high into adlb_tox
filtered_adlb_tox <-
  bind_rows(
    filtered_adlb_low,
    filtered_adlb_high
  ) |>
  select(-c(ATOXGR, ATOXGRN)) |>
  inner_join(adsl)

### filter for timepoints (either per visit or worst over a time period (overall,period,phase,...))
## here: use worst overall: ANL04FL/ANL05FL --- ANLHLFL
## if worst per period/phase:use ANL07/8 and ANL9/10 --- and ensure to create a combined version ANL078FL, ANL0910FL in the above code
filtered_adlb_tox <-
  filtered_adlb_tox |>
  filter(ANLHLFL == "Y")

## add the worst Grade to ATOXGRLH,BTOXGRLH, For BTOXGRLH add an extra level (for display purpose)
filtered_adlb_tox <-
  filtered_adlb_tox |>
  mutate(
    ATOXGRLH = factor(paste("Grade", ATOXGRLH), levels = paste("Grade", 0:4)),
    BTOXGRLH = factor(
      paste("Grade", BTOXGRLH),
      levels = c("N", paste("Grade", 0:4))
    )
  )

# Filter to non-missing PARCAT4 only
filtered_adlb_tox <- filtered_adlb_tox |>
  filter(PARCAT4 == "Graded tests")

# Sort ATOXDSCLH alphabetically
### Sorting: alphabetically by base term, LOW before HIGH within same term
atoxdsclh_levels <- filtered_adlb_tox |>
  distinct(ATOXDSCLH, ATOXDIR) |>
  mutate(
    base_term = sub(", (low|high)$", "", ATOXDSCLH, ignore.case = TRUE),
    ATOXDIR_ord = factor(ATOXDIR, levels = c("LOW", "HIGH"))
  ) |>
  arrange(base_term, ATOXDIR_ord) |>
  pull(ATOXDSCLH) |>
  as.character()


filtered_adlb_tox <- filtered_adlb_tox |>
  mutate(ATOXDSCLH = factor(as.character(ATOXDSCLH), levels = atoxdsclh_levels))

## trick for alt_counts_df to work with col splitting
# add BNRIND to adsl, all assign to extra level N (column will be used for N counts)
adsl <- adsl |>
  mutate(BTOXGRLH = "N") |>
  mutate(BTOXGRLH = factor(BTOXGRLH, levels = c("N", paste("Grade", 0:4))))

## add variable for column split header
filtered_adlb_tox$BTOXGRLH_header <- "Baseline Toxicity Grade"
adsl$BTOXGRLH_header <- "Baseline Toxicity Grade"

filtered_adlb_tox$BTOXGRLH_header2 <- " " ## first column N should not appear under Baseline column span
adsl$BTOXGRLH_header2 <- " " ## first column N should not appear under Baseline column span


check_categories <- unique(
  filtered_adlb_tox |> select(PARCAT4, ATOXDSCLH)
) |>
  arrange(PARCAT4, ATOXDSCLH)


################################################################################
# Define layout and build table:
################################################################################

lyt <- basic_table(show_colcounts = FALSE) |>
  ## to ensure N column is not under the Baseline column span header
  split_cols_by("BTOXGRLH_header2") |>
  split_cols_by("BTOXGRLH", split_fun = keep_split_levels("N")) |>
  split_cols_by("BTOXGRLH_header", nested = FALSE) |>
  split_cols_by(
    "BTOXGRLH",
    split_fun = make_split_fun(
      pre = list(rm_levels(excl = "N")),
      post = list(add_overall_facet("TOTAL", "Total"))
    )
  ) |>
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
  ) |>
  ## main part of table, restart row-split so nested = FALSE

  split_rows_by(
    "ATOXDSCLH",
    label_pos = "topleft",
    child_labels = "visible",
    split_label = "Laboratory Test",
    split_fun = drop_split_levels,
    section_div = " "
  ) |>
  split_rows_by(
    trtvar,
    label_pos = "hidden",
    split_label = "Treatment Group",
    section_div = " "
  ) |>
  ### a_freq_j
  ### the special statistic "n_rowdf" option does the trick here of getting the proper value for the N column
  summarize_row_groups(
    trtvar,
    cfun = a_freq_j,
    extra_args = list(
      .stats = "n_rowdf",
      restr_columns = c("N")
    ),
  ) |>
  ## add extra level TOTAL using new_levels, rather than earlier technique
  ## advantage for denominator derivation -- n_rowdf can be used, if we'd like to present fraction as well
  ## switch .stats to count_unique_denom_fraction or count_unique_fraction
  analyze(
    "ATOXGRLH",
    var_labels = if (show_pct) "Worst toxicity grade, n (%)" else "Worst toxicity grade, n",
    show_labels = "visible",
    indent_mod = 1L,
    afun = a_freq_j,
    extra_args = list(
      .stats = if (show_pct) "count_unique_fraction" else "count_unique",
      denom = "n_rowdf",
      new_levels = list(
        c("Total"),
        list(c("Grade 0", "Grade 1", "Grade 2", "Grade 3", "Grade 4"))
      ),
      new_levels_after = TRUE,
      .indent_mods = 1L,
      restr_columns = c(
        c("GRADE 0", "GRADE 1", "GRADE 2", "GRADE 3", "GRADE 4", "TOTAL")
      )
    )
  )

if (nrow(filtered_adlb_tox) > 0) {
  result <- build_table(lyt, filtered_adlb_tox, alt_counts_df = adsl, round_type = "sas")
} else {
  result <- NULL
}

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table
################################################################################


colwidth <- c(54, 7, 7, 7, 7, 5, 5, 7)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, label_width_ins = 2.4, orientation = "landscape")
