# Test the single variant for LBT01

adsl <- adsl_raw
adlb <- adlb_raw

adsl <- df_explicit_na(adsl)
adlb <- df_explicit_na(adlb) %>%
  filter(ANL01FL == "Y")

adlb_f <- adlb %>%
  dplyr::filter(
    PARAM == "Alanine Aminotransferase Measurement" &
      !(ACTARM == "B: Placebo" & AVISIT == "WEEK 1 DAY 8") &
      AVISIT != "SCREENING"
  )

testthat::test_that("LBT01 default variant is produced correctly", {
  # Define the split function
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
    split_rows_by("PARAM", split_fun = split_fun, label_pos = "topleft", split_label = obj_label(adlb_f$PARAM)) %>%
    split_rows_by("AVISIT", split_fun = split_fun, label_pos = "topleft", split_label = obj_label(adlb_f$AVISIT)) %>%
    split_cols_by_multivar(
      vars = c("AVAL", "CHG"),
      varlabels = c("Value at Visit", "Change from\nBaseline")
    ) %>%
    analyze_colvars(afun = afun)

  result <- build_table(lyt, adlb_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
