adsl <- adsl_raw
adlb <- adlb_raw

adlb <- adlb %>%
  mutate(
    BTOXGR = case_when(
      as.character(BTOXGR) == "" ~ "0",
      TRUE ~ as.character(BTOXGR)
    )
  ) %>%
  filter(ATOXGR %in% as.character(c(-4:4))) %>%
  filter(BTOXGR %in% as.character(c(-4:4))) %>%
  mutate(
    BTOXGR = factor(BTOXGR, levels = c(-4:4)),
    ATOXGR = factor(ATOXGR, levels = c(-4:4)),
    PARCAT1 = LBCAT
  )

adlb <- adlb %>%
  mutate(
    ATOXGR_CAT = forcats::fct_collapse(ATOXGR,
      "LOW" = c("-3", "-4"),
      "MODERATE/NORMAL" = c("-2", "-1", "0", "1", "2"),
      "HIGH" = c("3", "4")
    ),
    BTOXGR_CAT = forcats::fct_collapse(BTOXGR,
      "LOW" = c("-3", "-4"),
      "MODERATE/NORMAL" = c("-2", "-1", "0", "1", "2"),
      "HIGH" = c("3", "4")
    )
  ) %>%
  filter(ONTRTFL == "Y")

adlb_alt_cut <- adlb %>%
  mutate(
    ATOXGR_CAT = forcats::fct_collapse(ATOXGR,
      "LOW" = c("-2", "-3", "-4"),
      "MODERATE/NORMAL" = c("-1", "0", "1"),
      "HIGH" = c("2", "3", "4")
    ),
    BTOXGR_CAT = forcats::fct_collapse(BTOXGR,
      "LOW" = c("-2", "-3", "-4"),
      "MODERATE/NORMAL" = c("-1", "0", "1"),
      "HIGH" = c("2", "3", "4")
    )
  ) %>%
  filter(ONTRTFL == "Y")

adlb <- adlb %>% var_relabel(
  PARCAT1 = "Category for Lab Test",
  PARAM = "Parameter"
)

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by(var = "ARM") %>%
  add_overall_col("All Patients") %>%
  split_rows_by("PARCAT1", split_fun = drop_split_levels, label_pos = "topleft", split_label = obj_label(adlb$PARCAT1)) %>%
  split_rows_by("PARAM", split_fun = drop_split_levels, label_pos = "topleft", split_label = obj_label(adlb$PARAM)) %>%
  count_abnormal(
    var = "ATOXGR_CAT",
    abnormal = list(low = "LOW", high = "HIGH"),
    variables = list(id = "USUBJID", baseline = "BTOXGR_CAT"),
    exclude_base_abn = TRUE,
    .indent_mods = 4L
  ) %>%
  append_topleft("            Direction of Abnormality")

testthat::test_that("LBT15 variant 1 works as expected", {
  result <- build_table(lyt, adlb, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("LBT15 variant 2 works as expected", {
  result <- build_table(lyt, adlb_alt_cut, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
