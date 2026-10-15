library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)
library(haven)
library(rlang)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSFECG04"
fileid <- write_path(opath, tblid)
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

popfl <- "SAFFL"
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"

## selection of QTC parameters
selparamcd <- c("QTCBAG", "QTCFAG", "QTCS", "QTCLAG")

################################################################################
# initial read of data
################################################################################

################################################################################
# Process Data:
################################################################################

adsl <- adsl_jnj |>
  filter(.data[[popfl]] == "Y") |>
  select(STUDYID, USUBJID, all_of(c(trtvar, popfl))) |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c(
        "Xanomeline Low Dose",
        "Xanomeline High Dose",
        "Placebo"
      )
    )
  ) |>
  create_colspan_var(
    non_active_grp = ctrl_grp,
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = trtvar
  )

adeg_complete <- adeg_jnj

### available QTC parameters in study
selparamcd <- intersect(selparamcd, unique(adeg_complete$PARAMCD))


adeg <- adeg_complete |>
  filter(!is.na(USUBJID), .data[[popfl]] == "Y", !is.na(.data[[trtvar]]), PARAMCD %in% selparamcd, ANL03FL == "Y") |>
  ### Maximum On-treatment
  ### note: by filter ANL03FL, this table is restricted to On-treatment values, per definition of ANL03FL
  ### therefor, no need to add ONTRTFL in filter
  ### if derivation of ANL03FL is not restricted to ONTRTFL records, adding ONTRTFL here will not give the correct answer either
  ### as mixing worst with other period is not giving the proper selection !!!
  select(
    STUDYID,
    USUBJID,
    PARAM,
    PARAMN,
    PARAMCD,
    AVISIT,
    AVAL,
    BASE,
    AVALCAT1,
    BASECAT1,
    ONTRTFL,
    ANL03FL,
    all_of(trtvar)
  ) |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c(
        "Xanomeline Low Dose",
        "Xanomeline High Dose",
        "Placebo"
      )
    )
  )

# restrict to these - ordered by PARAMN
selparamcd <- as.character(
  adeg |> arrange(PARAMN) |> pull(PARAMCD) |> unique()
)

adeg <- adeg |>
  mutate(
    PARAMCD := factor(PARAMCD, levels = selparamcd),
    PARAM := factor(PARAM),
    AVISIT = factor("Maximum corrected QT interval, n", levels = c("Maximum corrected QT interval, n"))
  ) |>
  inner_join(adsl, by = c("STUDYID", "USUBJID", trtvar))


check1 <- adeg |>
  group_by(TRT01A, PARAMCD, AVISIT) |>
  summarize(n = n_distinct(USUBJID))


## add variable for column split header
adeg$BASECAT1_header <- "Baseline Corrected QT Interval"
adeg$BASECAT1_header2 <- " " ## first column N should not appear under Baseline column span

adeg$BASECAT1_header3 <- " " ## extra to allow for additional topleft material

###
AVALCAT1_levels <- levels(adeg$AVALCAT1)


## add extra level N to Basecat1
adeg <- adeg |>
  mutate(
    BASECAT1 = factor(as.character(BASECAT1), levels = c("N", AVALCAT1_levels))
  )


## trick for alt_counts_df to work with col splitting
# add BASECAT1 to adsl, all assign to extra level N (column will be used for N counts)
adslx <- adsl |>
  mutate(BASECAT1 = "N") |>
  mutate(BASECAT1 = factor(BASECAT1, levels = c("N", AVALCAT1_levels)))


adslx$BASECAT1_header <- "Baseline Corrected QT Interval"
adslx$BASECAT1_header2 <- " "
adslx$BASECAT1_header3 <- " " ## extra to allow for additional topleft material


################################################################################
# Define layout and build table:
################################################################################

lyt <- basic_table(show_colcounts = FALSE) |>
  split_cols_by("BASECAT1_header3") |>
  ## to ensure N column is not under the Baseline column span header
  split_cols_by("BASECAT1_header2") |>
  split_cols_by("BASECAT1", split_fun = keep_split_levels("N")) |>
  ## restart column split (Nested = False)
  ## Combined levels will be made, and the N column should not appear
  split_cols_by("BASECAT1_header", nested = FALSE) |>
  split_cols_by(
    "BASECAT1",
    split_fun = make_split_fun(
      pre = list(rm_levels(excl = "N")),
      post = list(
        add_overall_facet("TOTAL", "Total")
      )
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
  # ## main part of table
  split_rows_by(
    "PARAMCD",
    labels_var = "PARAM",
    nested = FALSE,
    label_pos = "topleft",
    child_labels = "visible",
    split_label = "QTc Interval",
    split_fun = drop_split_levels,
    section_div = " "
  ) |>
  split_rows_by(
    trtvar,
    label_pos = "topleft",
    indent_mod = 0L,
    child_labels = "hidden",
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
    indent_mod = 0L
  ) |>
  split_rows_by(
    "AVISIT",
    label_pos = "hidden",
    indent_mod = 0L,
    split_label = " ",
    child_labels = "visible",
    section_div = " "
  ) |>
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
      .indent_mods = 0L,
      restr_columns = c(
        toupper(AVALCAT1_levels),
        "TOTAL"
      )
    )
  )

result <- build_table(lyt, adeg, alt_counts_df = adslx, round_type = "sas")

################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

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


colwidth <- c(60, 7, 7, 5, 5, 5, 7)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid)
