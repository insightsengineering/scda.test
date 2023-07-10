adsl <- adsl_raw
adqs <- adqs_raw

adsl <- df_explicit_na(adsl)
adqs <- df_explicit_na(adqs)

adqs <- adqs %>%
  dplyr::filter(
    PARAM == "BFI All Questions",
    AVISIT != "SCREENING"
  )

split_fun <- drop_split_levels

testthat::test_that("CFBT01 default variant is produced correctly", {
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

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by(
      "PARAM",
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adqs$PARAM)
    ) %>%
    split_rows_by(
      "AVISIT",
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adqs$AVISIT)
    ) %>%
    split_cols_by_multivar(
      vars = c("AVAL", "CHG"),
      varlabels = c("Value at Visit", "Change from\nBaseline")
    ) %>%
    analyze_colvars(afun = afun)

  result <- build_table(lyt, adqs)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
