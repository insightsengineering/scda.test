# Test the single variant for EGT05_QTCAT

adsl <- adsl_pharmaverse
adeg <- adeg_pharmaverse

adsl <- df_explicit_na(adsl)
adeg <- df_explicit_na(adeg)

adeg_labels <- var_labels(adeg)
adeg_f <- adeg %>%
  filter(
    PARAMCD == "QT",
    ANL01FL == "Y",
    EGTPT == 1
  ) %>%
  mutate(
    AVALCAT1 = case_when(
      AVAL <= 450 ~ "<=450 msec",
      AVAL <= 480 ~ ">450 to <=480 msec",
      AVAL <= 500 ~ ">480 to <= 500 msec",
      AVAL > 500 ~ ">500 msec",
      is.na(AVAL) ~ "<Missing>"
    ),
    CHGCAT1 = case_when(
      CHG <= 30 ~ "<=30 msec",
      CHG <= 60 ~ ">30 to <=60 msec",
      CHG > 60 ~ ">60 msec",
      is.na(CHG) ~ "<Missing>"
    )
  ) %>%
  mutate(
    AVALCAT1 = factor(
      AVALCAT1,
      levels = c(
        "<=450 msec",
        ">450 to <=480 msec",
        ">480 to <= 500 msec",
        ">500 msec",
        "<Missing>"
      )
    ),
    CHGCAT1 = factor(
      CHGCAT1,
      levels = c(
        "<=30 msec",
        ">30 to <=60 msec",
        ">60 msec",
        "<Missing>"
      )
    )
  ) %>%
  var_relabel(
    AVALCAT1 = "Value at Visit",
    CHGCAT1 = "Change from Baseline"
  )

levels(adeg_f$AVISIT) <- c(
  "Baseline", "Week 2", "Week 4", "Week 6", "Week 8", "Week 12", "Week 16", "Week 20", "Week 24", "Week 26", "<Missing>"
)

testthat::test_that("EGT05_QTCAT default variant is produced correctly", {
  split_fun <- drop_split_levels

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    split_rows_by(
      "PARAM",
      split_label = obj_label(adeg_f$PARAM),
      split_fun = split_fun,
      label_pos = "topleft"
    ) %>%
    split_rows_by(
      "AVISIT",
      split_label = obj_label(adeg_f$AVISIT),
      split_fun = split_fun,
      label_pos = "topleft"
    ) %>%
    analyze_vars(
      vars = c("AVALCAT1", "CHGCAT1"),
      var_labels = c("Value at Visit", "Change from Baseline")
    ) %>%
    append_topleft("    Category")

  result <- build_table(lyt = lyt, df = adeg_f, alt_counts_df = adsl) %>%
    prune_table()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
