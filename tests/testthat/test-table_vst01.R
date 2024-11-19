# Test the single variant for VST01

adsl <- adsl_pharmaverse
advs <- advs_pharmaverse

adsl <- df_explicit_na(adsl)
advs <- df_explicit_na(advs)

advs_label <- var_labels(advs)

advs <- advs %>%
  filter(PARAMCD == "DIABP") %>%
  mutate(
    PARAMCD = droplevels(PARAMCD),
    PARAM = droplevels(PARAM)
  )

advs_pb <- advs %>%
  filter(ABLFL != "Y")

advs_pb_max <- advs_pb %>%
  group_by(PARAM, USUBJID) %>%
  arrange(desc(AVAL)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(AVISIT = "Post-Baseline Maximum")

advs_pb_min <- advs_pb %>%
  group_by(PARAM, USUBJID) %>%
  arrange(AVAL) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(AVISIT = "Post-Baseline Minimum")

advs_pb_last <- advs_pb %>%
  group_by(PARAM, USUBJID) %>%
  arrange(desc(AVISITN)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(AVISIT = "Post-Baseline Last")

advs_f <- rbind(
  advs,
  advs_pb_last,
  advs_pb_min,
  advs_pb_max
)

advs_f <- advs_f %>%
  mutate(AVISIT = droplevels(AVISIT))
levels(advs_f$AVISIT) <- c(
  "Baseline", "Week 2", "Week 4", "Week 6", "Week 8", "Week 12", "Week 16", "Week 20", "Week 24", "Week 26",
  "End of Treatment", "Post-Baseline Minimum", "Post-Baseline Maximum", "Post-Baseline Last", "<Missing>"
)

var_labels(advs_f) <- advs_label

testthat::test_that("VST01 default variant is produced correctly", {
  skip_if_too_deep(3)

  split_fun <- drop_split_levels

  afun <- function(x, .var, .spl_context, ...) {
    n_fun <- sum(!is.na(x), na.rm = TRUE)
    if (n_fun == 0) {
      mean_sd_fun <- c(NA, NA)
      median_fun <- NA
      min_max_fun <- c(NA, NA)
    } else {
      mean_sd_fun <- c(mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
      median_fun <- median(x, na.rm = TRUE)
      min_max_fun <- c(min(x), max(x))
    }
    is_chg <- .var == "CHG"
    is_baseline <- .spl_context$value[which(.spl_context$split == "AVISIT")] == "BASELINE"
    if (is_baseline && is_chg) n_fun <- mean_sd_fun <- median_fun <- min_max_fun <- NULL

    in_rows(
      "n" = n_fun,
      "Mean (SD)" = mean_sd_fun,
      "Median" = median_fun,
      "Min - Max" = min_max_fun,
      .formats = list("n" = "xx", "Mean (SD)" = "xx.xx (xx.xx)", "Median" = "xx.xx", "Min - Max" = "xx.xx - xx.xx"),
      .format_na_strs = list("n" = "NE", "Mean (SD)" = "NE (NE)", "Median" = "NE", "Min - Max" = "NE - NE")
    )
  }

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_rows_by("AVISIT", split_fun = split_fun, label_pos = "topleft", split_label = "\n\nAnalysis Visit") %>%
    split_cols_by_multivar(
      vars = c("AVAL", "CHG"),
      varlabels = c("Value at Visit", "Change from\nBaseline")
    ) %>%
    analyze_colvars(afun = afun)

  result <- build_table(lyt = lyt, df = advs_f, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
