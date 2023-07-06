adsl <- adsl_raw
adlb <- adlb_raw

adsl <- df_explicit_na(adsl)
adlb <- df_explicit_na(adlb)

qntls <- adlb %>%
  group_by(PARAMCD) %>%
  summarise(as_tibble(t(quantile(AVAL, probs = c(0.1, 0.9)))), .groups = "drop_last") %>%
  rename(q1 = 2, q2 = 3)

adlb <- adlb %>%
  left_join(qntls, by = "PARAMCD")

set.seed(1)

# Modify ANRIND and create AVALCAT1/PARCAT2
# PARCAT2 is just used for filtering, but in order to be the
# filtering as realistic as possible, will create the variable.
adlb <- adlb %>%
  mutate(
    ANRIND = factor(
      case_when(
        ANRIND == "LOW" & AVAL <= q1 ~ "LOW LOW",
        ANRIND == "HIGH" & AVAL >= q2 ~ "HIGH HIGH",
        TRUE ~ as.character(ANRIND)
      ),
      levels = c("", "HIGH", "HIGH HIGH", "LOW", "LOW LOW", "NORMAL")
    ),
    AVALCAT1 = factor(
      case_when(
        ANRIND %in% c("HIGH HIGH", "LOW LOW") ~
          sample(x = c("LAST", "REPLICATED", "SINGLE"), size = n(), replace = TRUE, prob = c(0.3, 0.6, 0.1)),
        TRUE ~ ""
      ),
      levels = c("", c("LAST", "REPLICATED", "SINGLE"))
    ),
    PARCAT2 = factor(ifelse(ANRIND %in% c("HIGH HIGH", "LOW LOW"), "LS",
                            sample(c("SI", "CV", "LS"), size = n(), replace = TRUE)
    ))
  ) %>%
  select(-q1, -q2)

# Pre-processing steps
adlb_f <- adlb %>%
  filter(ONTRTFL == "Y" & PARCAT2 == "LS" & SAFFL == "Y" & !is.na(AVAL)) %>%
  mutate(abn_dir = factor(case_when(
    ANRIND == "LOW LOW" ~ "Low",
    ANRIND == "HIGH HIGH" ~ "High",
    TRUE ~ ""
  ), levels = c("Low", "High", ""))) %>%
  df_explicit_na()

# Construct analysis map
map <- expand.grid(
  PARAM = levels(adlb$PARAM),
  abn_dir = c("Low", "High"),
  stringsAsFactors = FALSE
) %>%
  arrange(PARAM, desc(abn_dir))

testthat::test_that("LBT05 variant 1 is produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_rows_by(
      "PARAM",
      label_pos = "topleft",
      split_label = "Laboratory Test"
    ) %>%
    summarize_num_patients(var = "USUBJID", .stats = "unique_count") %>%
    split_rows_by("abn_dir", split_fun = trim_levels_to_map(map)) %>%
    count_abnormal_by_marked(
      var = "AVALCAT1",
      variables = list(id = "USUBJID", param = "PARAM", direction = "abn_dir")
    ) %>%
    append_topleft("  Direction of Abnormality")

  result <- build_table(lyt, df = adlb_f, alt_counts_df = adsl)

  has_lbl <- function(lbl) CombinationFunction(function(tr) obj_label(tr) == lbl || sum(unlist(row_values(tr))) != 0)
  result <- prune_table(result, keep_rows(has_lbl("Any Abnormality")))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("LBT05 variant 2 is produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_rows_by(
      "PARAM",
      label_pos = "topleft",
      split_label = "Laboratory Test"
    ) %>%
    summarize_num_patients(var = "USUBJID", .stats = "unique_count") %>%
    split_rows_by("abn_dir", split_fun = trim_levels_to_map(map)) %>%
    count_abnormal_by_marked(
      var = "AVALCAT1",
      variables = list(id = "USUBJID", param = "PARAM", direction = "abn_dir")
    ) %>%
    append_topleft("  Direction of Abnormality")

  result <- build_table(lyt, df = adlb_f, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("LBT05 variant 4 is produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_rows_by(
      "PARAM",
      label_pos = "topleft",
      split_label = "Laboratory Test",
      split_fun = trim_levels_in_group("abn_dir", drop_outlevs = TRUE)
    ) %>%
    summarize_num_patients(var = "USUBJID", .stats = "unique_count") %>%
    split_rows_by("abn_dir") %>%
    count_abnormal_by_marked(
      var = "AVALCAT1",
      variables = list(id = "USUBJID", param = "PARAM", direction = "abn_dir")
    ) %>%
    append_topleft("  Direction of Abnormality")

  result <- build_table(lyt, df = adlb_f, alt_counts_df = adsl)
  result <- result %>% prune_table()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
