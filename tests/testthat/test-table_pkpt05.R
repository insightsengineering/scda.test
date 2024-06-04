# Data generation
adpp <- adpp_pharmaverse
adpp_urine <- adpp %>% dplyr::filter(PPSPEC == "URINE", AVISIT == "Day 1")

# Helper function
threesigfmt <- function(x, ...) {
  as.character(signif(x, 3))
}

# Define template layout
l <- basic_table() %>%
  split_rows_by(
    var = "ARMCD",
    split_fun = trim_levels_in_group("ARMCD"),
    label_pos = "topleft",
    split_label = "Treatment Arm"
  ) %>%
  split_rows_by(
    var = "PPTEST",
    label_pos = "topleft",
    split_label = "PK Parameter",
    child_labels = "hidden"
  ) %>%
  analyze_vars_in_cols(
    vars = "AVAL",
    .stats = c(
      "n", "mean", "sd", "cv",
      "geom_mean", "geom_cv", "median",
      "min", "max"
    ),
    .labels = c(
      n = "n",
      mean = "Mean",
      sd = "SD",
      cv = "CV (%)",
      geom_mean = "Geometric Mean",
      geom_cv = "CV % Geometric Mean",
      median = "Median",
      min = "Minimum",
      max = "Maximum"
    ),
    .formats = c(
      n = "xx.",
      mean = threesigfmt,
      sd = threesigfmt,
      cv = "xx.x",
      median = threesigfmt,
      geom_mean = threesigfmt,
      geom_cv = "xx.x",
      min = threesigfmt,
      max = threesigfmt
    )
  )

# PKPT05 Drug X
testthat::test_that("PKPT05 Drug X is produced correctly", {
  adpp0 <- adpp_urine %>%
    filter(PPCAT == "XANOMELINE") %>%
    # h_pkparam_sort() %>%
    dplyr::mutate(PKPARAM = factor(paste0(PPTEST, " (", AVALU, ")")))
  result <- build_table(l, df = adpp0)
  main_title(result) <- paste("Summary of", unique(adpp0$PPSPEC), "PK Parameter by Treatment Arm, PK Population")
  subtitles(result) <- paste("Analyte:", unique(adpp0$PPCAT), "\nVisit:", unique(adpp0$AVISIT))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
