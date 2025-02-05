testthat::test_that("PKCL01 listing is produced correctly", {
  adpc <- adpc_pharmaverse
  analyte <- "Pharmacokinetic concentration of Xanomeline"
  out <- adpc %>%
    filter(PARAM == analyte) %>%
    select(ARM, USUBJID, VISIT, NFRLT, AFRLT, AVAL)

  var_labels(out) <- c(
    ARM = "Treatment Group",
    USUBJID = "Subject ID",
    VISIT = "Visit",
    NFRLT = paste0("Nominal Sampling\nTime (hr)"),
    AFRLT = paste0("Actual Time\nFrom First\nDose (hr)"),
    AVAL = paste0("Concentration\n(", adpc$AVALU[1], ")")
  )

  result <- as_listing(
    out,
    key_cols = c("ARM", "USUBJID", "VISIT"),
    disp_cols = names(out),
    main_title = paste(
      "Listing of", analyte, "Concentration by Treatment Group, Subject and Nominal Time,",
      "PK Population\nProtocol: xxnnnnn"
    ),
    subtitles = paste("Analyte:", analyte)
  ) %>% head(50)

  testthat::expect_snapshot(result)
})

testthat::test_that("PKCL01 listing key columns values are repeated when entries span multiple pages", {
  adpc <- adpc_pharmaverse
  out <- adpc %>%
    select(TRT01A, USUBJID, VISIT, NFRLT, AFRLT, AVAL)

  var_labels(out) <- c(
    TRT01A = "Treatment Group",
    USUBJID = "Subject ID",
    VISIT = "Visit",
    NFRLT = paste0("Nominal Sampling\nTime (", adpc$FRLTU[1], ")"),
    AFRLT = paste0("Actual Time\nFrom First\nDose (", adpc$FRLTU[1], ")"),
    AVAL = paste0("Concentration\n(", adpc$AVALU[1], ")")
  )

  out  <- out %>%
    arrange(TRT01A, USUBJID, NFRLT)

  result <- as_listing(
    out,
    key_cols = c("TRT01A", "USUBJID", "VISIT"),
    disp_cols = names(out)
  ) %>%
    head(50) %>%
    export_as_txt(tf_wrap = TRUE, cpp = 120, lpp = 50)

  testthat::expect_snapshot(result %>% cat())
})
