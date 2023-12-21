adpc <- adpc_pharmaverse

testthat::test_that("PKCT01 is produced correctly", {
  l <- basic_table() %>%
    split_rows_by(
      var = "ARM",
      split_fun = keep_split_levels(c(
        "Xanomeline High Dose",
        "Xanomeline Low Dose"
      ))
    ) %>%
    split_rows_by(var = "PARAM", child_labels = "hidden") %>%
    analyze_vars_in_cols(vars = "AVAL")

  result <- build_table(l, df = adpc)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  l2 <- basic_table() %>%
    split_rows_by(
      var = "ARM",
      split_fun = keep_split_levels(c(
        "Xanomeline High Dose",
        "Xanomeline Low Dose"
      ))
    ) %>%
    split_rows_by(var = "PARAM", child_labels = "hidden") %>%
    analyze_vars_in_cols(
      vars = "AVALCAT1", var_type = "character", .stats = c("n_blq"),
      .labels = c(n_blq = "n_blq")
    )

  adpc <- adpc %>% mutate(AVALCAT1 = as.factor(AVALCAT1))
  result <- build_table(l2, df = adpc)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("Specific PKCT01 features are present", {
  # Helper function
  threesigfmt <- function(x, ...) {
    as.character(signif(x, 3))
  }

  # Setting up the data
  adpc_1 <- adpc %>%
    mutate(
      NFRLT = as.factor(NFRLT),
      AVALCAT1 = as.factor(AVALCAT1)
    ) %>%
    filter(ACTARM %in% c("Xanomeline High Dose")) %>%
    mutate(ACTARM = factor(ACTARM, levels = c("Xanomeline High Dose"))) %>%
    select(NFRLT, ACTARM, VISIT, AVAL, PARAM, AVALCAT1)

  # Row structure
  l_rows <- basic_table() %>%
    split_rows_by(
      var = "ACTARM",
      split_label = "Cohort/Treatment",
      label_pos = "topleft"
    ) %>%
    split_rows_by(
      var = "VISIT",
      split_label = "Visit",
      label_pos = "topleft"
    ) %>%
    split_rows_by(
      var = "NFRLT",
      split_label = "Norminal Time from First Dose",
      label_pos = "topleft",
      child_labels = "hidden"
    )

  # Column results for numeric values
  lyt <- l_rows %>%
    analyze_vars_in_cols(
      vars = c("AVAL", "AVALCAT1", rep("AVAL", 8)),
      .stats = c(
        "n", "n_blq", "mean", "sd", "cv",
        "geom_mean", "geom_cv", # "geom_mean_ci",
        "median", "min", "max"
      ),
      .formats = c(
        n = "xx.",
        n_blq = "xx.",
        mean = threesigfmt,
        sd = threesigfmt,
        cv = "xx.x",
        median = threesigfmt,
        geom_mean = threesigfmt,
        geom_cv = "xx.x",
        min = threesigfmt,
        max = threesigfmt
      ),
      .labels = c(
        n = "n",
        n_blq = "Number\nof\n<LTR/BLQ>s",
        mean = "Mean",
        sd = "SD",
        cv = "CV (%) Mean",
        geom_mean = "Geometric Mean",
        geom_cv = "CV % Geometric Mean",
        median = "Median",
        min = "Minimum",
        max = "Maximum"
      ),
      na_str = "NE"
    )
  result <- build_table(lyt, df = adpc_1) %>% prune_table()

  # Decorating
  main_title(result) <- "Summary of PK Concentrations by Nominal Time and Treatment: PK Evaluable\n Protocol: xxxxx"
  subtitles(result) <- paste("Analyte: ", unique(unique(adpc$PARAM)), "Treatment:", unique(unique(adpc$ACTARM)))
  main_footer(result) <- "NE: Not Estimable"

  # Values are correct
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Comparing
  string_res <- strsplit(toString(matrix_form(result, TRUE)), "\n")[[1]]

  # Checking NAs are NEs
  testthat::expect_false(any(sapply(string_res, grepl, pattern = "NA")))
  testthat::expect_equal(sum(sapply(string_res, grepl, pattern = "NE")), 3L)

  # Checking significative digits (DISCLAIMER: this is an hack and is NOT well supported)
  mean_vals <- rtables::cell_values(result,
    rowpath = NULL,
    colpath = c("multivars", "AVAL._[[2]]_.")
  ) %>%
    unlist()
  names(mean_vals) <- NULL
  tmp_result <- sapply(seq_len(nrow(result)), function(x) matrix_form(result[x, 3])$strings[2, 2])
  tmp_result <- tmp_result[tmp_result != ""]
  testthat::expect_equal(tmp_result, as.character(signif(mean_vals, 3)))

  # Pagination works roughly
  pag_works <- paginate_table(result, verbose = FALSE, lpp = 21)
  testthat::expect_equal(length(pag_works), 6L)
})
