testthat::test_that("PKCL01 listing is produced correctly", {
  adpc <- adpc_raw
  analyte <- "Plasma Drug X"
  out <- adpc %>%
    filter(PARAM == analyte)%>%
    select(ARM, USUBJID, VISIT, NRELTM1, ARELTM1, AVAL)

  formatters::var_labels(out) <- c(
    ARM = 'Treatment Group',
    USUBJID = "Subject ID",
    VISIT = 'Visit',
    NRELTM1 = paste0('Nominal Sampling\nTime (', adpc$RELTMU[1], ')'),
    ARELTM1 = paste0('Actual Time\nFrom First\nDose (', adpc$RELTMU[1], ')'),
    AVAL = paste0('Concentration\n(', adpc$AVALU[1], ')'))

  testthat::expect_message(result <- as_listing(
    out,
    key_cols = c("ARM", "USUBJID", "VISIT"),
    disp_cols = names(out),
    main_title = paste("Listing of", analyte, "Concentration by Treatment Group, Subject and Nominal Time,",
                       "PK Population\nProtocol: xxnnnnn"),
    subtitles = paste("Analyte:", analyte)
  ), "sorting incoming data by key columns")

  testthat::expect_snapshot(result)
})
