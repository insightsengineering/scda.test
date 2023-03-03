testthat::test_that("CML02A_GL listing is produced correctly", {
  out <- adcm_raw %>%
    select(ATC2, CMDECOD, CMTRT) %>%
    unique()

  formatters::var_labels(out) <- c(
    ATC2 = "ATC Class Level 2",
    CMDECOD = "WHODrug Preferred Name",
    CMTRT = "Investigator-Specified\nTreatment Term"
  )

  testthat::expect_message(result <- as_listing(
    out,
    key_cols = c("ATC2", "CMDECOD", "CMTRT"),
    disp_cols = names(out),
    main_title = "Listing of Concomitant Medication Class Level 2, Preferred Name, and Investigator-Specified Terms"
  ), "sorting incoming data by key columns")

  testthat::expect_snapshot(result)
})
