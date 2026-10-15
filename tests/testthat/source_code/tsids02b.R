library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

tblid <- "TSIDS02b"
fileid <- write_path(opath, tblid)
popfl <- "ENRLFL"
trtvar <- "TRT01P"
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


################################################################################
# Process data:
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
    )
  ) |>
  select(
    STUDYID,
    USUBJID,
    !!rlang::sym(trtvar),
    !!rlang::sym(popfl),
    EOTSTT,
    DCTREAS,
    EOSSTT,
    DCSREAS,
  ) |>
  create_colspan_var(
    non_active_grp = "Placebo",
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = trtvar
  )

# Actual Dataset
addisp1 <- addisp_jnj |>
  filter(!!rlang::sym(popfl) == "Y") |>
  select(USUBJID, PARAMCD, AVALC)

# Create treatment names
addisp2 <- addisp_jnj |>
  filter(!!rlang::sym(popfl) == "Y" & !is.na(DSSCAT) & DSSCAT != "") |>
  select(USUBJID, DSSCAT)

a <- tolower(unique(addisp2$DSSCAT))


addisp <- addisp1 |>
  tidyr::pivot_wider(
    names_from = c(PARAMCD),
    values_from = AVALC
  ) |>
  mutate(
    ONGOING_SUB = case_when(
      rowSums(dplyr::across(dplyr::starts_with("EOTS"), ~ toupper(.x) == "ONGOING"), na.rm = TRUE) > 0 ~ "Y",
      TRUE ~ NA_character_
    ),
    COMPL_SUB = dplyr::case_when(
      rowSums(dplyr::across(dplyr::starts_with("EOTS"), ~ toupper(.x) == "COMPLETED"), na.rm = TRUE) ==
        rowSums(!is.na(dplyr::across(dplyr::starts_with("EOTS")))) &
        rowSums(!is.na(dplyr::across(dplyr::starts_with("EOTS")))) > 0 ~
        "Y",
      TRUE ~ NA_character_
    ),
    DISC_BOTH_TREATMENT = dplyr::case_when(
      rowSums(dplyr::across(dplyr::starts_with("EOTS"), ~ toupper(.x) == "DISCONTINUED"), na.rm = TRUE) ==
        rowSums(!is.na(dplyr::across(dplyr::starts_with("EOTS")))) &
        rowSums(!is.na(dplyr::across(dplyr::starts_with("EOTS")))) > 0 ~
        "Y",
      TRUE ~ NA_character_
    ),
    DISC_ONE_TREATMENT = dplyr::case_when(
      rowSums(dplyr::across(dplyr::starts_with("EOTS"), ~ toupper(.x) == "DISCONTINUED"), na.rm = TRUE) > 0 ~ "Y",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(
    dplyr::across(
      dplyr::starts_with("EOTS"),
      ~ dplyr::case_when(toupper(.x) == "COMPLETED" ~ "Y", TRUE ~ NA_character_),
      .names = "COMPLETED_{gsub('EOTS|STT', '', .col)}"
    ),
    dplyr::across(
      dplyr::starts_with("EOTS"),
      ~ dplyr::case_when(toupper(.x) == "DISCONTINUED" ~ "Y", TRUE ~ NA_character_),
      .names = "DISCONTINUED_{gsub('EOTS|STT', '', .col)}"
    )
  )


# for counting the treatments
count_trt <- grep("^EOTS", names(addisp), value = TRUE)

num_words <- c(
  "zero",
  "one",
  "both",
  "three",
  "four",
  "five",
  "six",
  "seven",
  "eight",
  "nine",
  "ten"
)

count_label <- num_words[length(count_trt) + 1]

addisp <- inner_join(addisp, adsl, by = c("USUBJID"))

# Ensure all pivoted AVALC columns share the same factor levels (mimics SAS format behavior)
avalc_cols <- grep("^(EOTS|DCTS|LTVIST)", names(addisp), value = TRUE)
all_levels <- sort(unique(unlist(lapply(addisp[avalc_cols], function(x) {
  if (is.factor(x)) levels(x) else unique(na.omit(x))
}))))
addisp <- addisp |>
  mutate(across(all_of(avalc_cols), ~ factor(.x, levels = all_levels)))


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

totdf <- tribble(
  ~valname                                                    ,
  ~label                                                      ,
  ~levelcombo                                                 ,
  ~exargs                                                     ,
  "Total"                                                     ,
  "Total"                                                     ,
  c("Xanomeline Low Dose", "Xanomeline High Dose", "Placebo") ,
  list()
)

rr_method <- "wald"
ref_path <- c("colspan_trt", " ", trtvar, "Placebo")
extra_args_rr <- list(
  riskdiff = FALSE,
  method = rr_method,
  ref_path = ref_path,
  .stats = c("count_unique_fraction")
)


map1 <- tribble(
  ~var                                               ,
  ~value                                             ,
  ~label                                             ,
  "ONGOING_SUB"                                      ,
  "Y"                                                ,
  "Subjects ongoing any treatment"                   ,
  "COMPL_SUB"                                        ,
  "Y"                                                ,
  "Completed treatment~[super a]"                    ,
  "DISC_BOTH_TREATMENT"                              ,
  "Y"                                                ,
  paste("Discontinued", count_label, "study agents") ,
  "DISC_ONE_TREATMENT"                               ,
  "Y"                                                ,
  paste("Discontinued one study agent")
)

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) |>
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) |>
  split_cols_by(trtvar) |>
  split_cols_by(
    trtvar,
    split_fun = add_combo_levels(totdf, keep_levels = "Total"),
    nested = FALSE
  ) |>
  split_rows_by(var = "STUDYID", section_div = "", child_labels = "hidden", parent_name = "STUDYID1") |>
  analyze(
    vars = c("ONGOING_SUB", "COMPL_SUB", "DISC_BOTH_TREATMENT", "DISC_ONE_TREATMENT"),
    afun = a_freq_j,
    extra_args = c(
      extra_args_rr,
      list(
        label_map = map1,
        val = "Y"
      )
    ),
    show_labels = "hidden"
  )


# calculate how many completed /discontinued treatments.

comp_vars <- grep("^COMPLETED_[0-9]+$", names(addisp), value = TRUE)
comp_vars <- comp_vars[order(as.integer(sub("COMPLETED_", "", comp_vars)))]

disc_vars <- grep("^DISCONTINUED_[0-9]+$", names(addisp), value = TRUE)
disc_vars <- disc_vars[order(as.integer(sub("DISCONTINUED_", "", disc_vars)))]


agents <- sort(unique(
  c(
    as.integer(sub("COMPLETED_", "", comp_vars)),
    as.integer(sub("DISCONTINUED_", "", disc_vars))
  )
))

agent_map <- setNames(a[seq_along(agents)], agents)


for (ag in agents) {
  stat_var <- paste0("EOTS", ag, "STT") # <-- dynamic status variable
  rs_var <- paste0("DCTS", ag, "RS") # <-- dynamic reason variable

  agent_lbl <- agent_map[[as.character(ag)]]
  if (is.null(agent_lbl) || is.na(agent_lbl) || agent_lbl == "") {
    # fallback so label never breaks
    agent_lbl <- paste0("Treatment ", ag)
  }

  ## ---- TREATMENT STATUS/REASONS (nested under discontinued) ----

  if (rs_var %in% names(addisp)) {
    xmap <- data.frame(value = levels(addisp[[rs_var]]), label = levels(addisp[[rs_var]]))
    xmap$label[xmap$value == "COMPLETED"] <- paste0("Completed treatment with ", agent_lbl)
    xmap$label[xmap$value == "DISCONTINUED"] <- paste0("Discontinued treatment with ", agent_lbl)

    lyt <- lyt |>
      split_rows_by(
        var = "STUDYID",
        section_div = "",
        child_labels = "hidden",
        nested = FALSE,
        parent_name = paste0("STUDYIDag", ag)
      ) |>
      split_rows_by(
        stat_var,
        split_fun = keep_split_levels(c("COMPLETED", "DISCONTINUED")),
        child_labels = "hidden",
        nested = TRUE
      ) |>
      analyze(
        vars = stat_var,
        afun = a_two_tier,
        extra_args = c(
          extra_args_rr,
          list(
            grp_fun = a_freq_j,
            detail_fun = a_freq_j,
            inner_var = rs_var,
            drill_down_levs = "DISCONTINUED",
            drop_levels = TRUE,
            label_map = xmap
          )
        )
      )
  }
}

## ---- STUDY STATUS/REASONS (nested under discontinued) ----
stat_var <- "EOSSTT"
rs_var <- "DCSREAS"

xmap <- data.frame(
  value = c(levels(addisp[[stat_var]]), levels(addisp[[rs_var]])),
  label = c(levels(addisp[[stat_var]]), levels(addisp[[rs_var]]))
)
xmap$label[xmap$value == "COMPLETED"] <- "Completed study"
xmap$label[xmap$value == "DISCONTINUED"] <- "Discontinued study"
xmap$label[xmap$value == "ONGOING"] <- "Subjects ongoing study"

lyt <- lyt |>
  split_rows_by(
    var = "STUDYID",
    section_div = "",
    child_labels = "hidden",
    nested = FALSE,
    parent_name = "STUDYIDlast"
  ) |>
  split_rows_by(
    stat_var,
    split_fun = keep_split_levels(c("ONGOING", "COMPLETED", "DISCONTINUED")),
    nested = TRUE,
    child_labels = "hidden"
  ) |>
  analyze(
    vars = stat_var,
    afun = a_two_tier,
    extra_args = c(
      grp_fun = a_freq_j,
      detail_fun = a_freq_j,
      inner_var = rs_var,
      drill_down_levs = "DISCONTINUED",
      list(drop_levels = TRUE, label_map = xmap, .stats = c("count_unique_fraction"))
    )
  )

result <- build_table(lyt, addisp, alt_counts_df = adsl, round_type = "sas")

################################################################################
# Post-Processing
################################################################################

# Add dynamic sorting for each agent's treatment status section
for (ag in agents) {
  stat_var <- paste0("EOTS", ag, "STT") # <-- dynamic status variable

  if (stat_var %in% names(addisp)) {
    result <- result |>
      sort_at_path(
        path = c(paste0("STUDYIDag", ag), "*", stat_var, "DISCONTINUED", stat_var),
        scorefun = jj_complex_scorefun(colpath = "Total", lastcat = "Other")
      )
  }
}

result <- result |>
  sort_at_path(
    path = c("STUDYIDlast", "*", "EOSSTT", "DISCONTINUED", "EOSSTT"),
    scorefun = jj_complex_scorefun(colpath = "Total", lastcat = "Other")
  )


################################################################################
# Add titles and footnotes:
################################################################################

result <- set_titles(result, tab_titles)

################################################################################
# Convert to tbl file and output table:
################################################################################


colwidth <- c(64, 21, 21, 21, 23)

tt_to_tlgrtf(colwidths = colwidth, result, file = fileid, orientation = "landscape")
