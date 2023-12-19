# Data generation
adpp <- adpp_pharmaverse
adpp_plasma <- adpp %>% dplyr::filter(PPSPEC == "PLASMA")

# Define template layout
l <- basic_table() %>%
  split_cols_by(
    var = "ARMCD",
    split_fun = trim_levels_in_group("ARMCD"),
    # label_pos = "topleft", # nolint
    split_label = "Treatment Arm"
  ) %>%
  split_rows_by(
    var = "PKPARAM",
    label_pos = "topleft",
    split_label = "PK Parameter"
  ) %>%
  analyze_vars(
    vars = "AVAL",
    .stats = c("n", "mean_sd", "cv", "geom_mean", "geom_cv", "median", "range"),
    .formats = c(
      n = "xx.",
      mean_sd = sprintf_format("%.3e (%.3e)"),
      cv = "xx.x",
      geom_mean = sprintf_format("%.3e"),
      geom_cv = "xx.x",
      median = sprintf_format("%.3e"),
      range = sprintf_format("%.3e - %.3e")
    )
  )

# PKPT02 COMPARTMENTAL
testthat::test_that("PKPT02 is produced correctly for Drug X", {
  adpp0 <- adpp_plasma %>%
    filter(PPCAT == "COMPARTMENTAL") %>%
    h_pkparam_sort() %>%
    mutate(PKPARAM = factor(paste0(TLG_DISPLAY, " (", PPORRESU, ")")))

  result <- build_table(l, df = adpp0)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
