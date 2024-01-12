# Tests LBT07

adsl <- adsl_pharmaverse
adlb <- adlb_pharmaverse %>%
  mutate(
    WGRLOFL = ifelse(AVISIT == "POST-BASELINE MINIMUM", "Y", ""),
    WGRHIFL = ifelse(AVISIT == "POST-BASELINE MAXIMUM", "Y", "")
  ) %>%
  filter(ATOXGR != "<Missing>")

adlb_labels <- var_labels(adlb)

# Ensure character variables are converted to factors and empty strings and NAs are explicit missing levels.
adsl <- df_explicit_na(adsl)
adlb <- df_explicit_na(adlb)

# Select worst post-baseline records.
adlb_f <- adlb %>%
  filter(ONTRTFL == "Y") %>%
  filter(WGRLOFL == "Y" | WGRHIFL == "Y")

var_labels(adlb_f) <- adlb_labels

# Derive GRADE_DIR and GRADE_ANL to use in layout from ATOXGR
adlb_f <- adlb_f %>%
  mutate(
    GRADE_DIR = factor(
      case_when(
        ATOXGR %in% c("-1", "-2", "-3", "-4") & .data$WGRLOFL == "Y" ~ "LOW",
        ATOXGR == "0" ~ "ZERO",
        ATOXGR %in% c("1", "2", "3", "4") & .data$WGRHIFL == "Y" ~ "HIGH",
        TRUE ~ "NONE"
      ),
      levels = c("LOW", "ZERO", "HIGH", "NONE")
    ),
    GRADE_ANL = forcats::fct_relevel(
      forcats::fct_recode(ATOXGR,
        `1` = "-1", `2` = "-2", `3` = "-3"
      ),
      c("0", "1", "2", "3")
    )
  ) %>%
  var_relabel(
    GRADE_DIR = "Direction of Abnormality",
    GRADE_ANL = "Analysis Grade"
  )

# Construct analysis map
map <- expand.grid(
  PARAM = levels(adlb$PARAM),
  GRADE_DIR = c("LOW", "HIGH"),
  GRADE_ANL = as.character(1:3),
  stringsAsFactors = FALSE
) %>%
  arrange(PARAM, desc(GRADE_DIR), GRADE_ANL)

testthat::test_that("LBT07 is produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_rows_by(
      "PARAM",
      label_pos = "topleft",
      split_label = obj_label(adlb_f$PARAM)
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      required = "ATOXGR",
      .stats = "unique_count"
    ) %>%
    split_rows_by(
      "GRADE_DIR",
      label_pos = "topleft",
      split_label = obj_label(adlb_f$GRADE_DIR),
      split_fun = trim_levels_to_map(map)
    ) %>%
    count_abnormal_by_worst_grade(
      var = "GRADE_ANL",
      variables = list(id = "USUBJID", param = "PARAM", grade_dir = "GRADE_DIR"),
      .indent_mods = 4L
    ) %>%
    append_topleft("            Highest NCI CTCAE Grade")

  result <- build_table(lyt = lyt, df = adlb_f, alt_counts_df = adsl)
  result <- result %>% prune_table()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
