testthat::test_that("CML02B_GL listing is produced correctly", {
  out <- adcm_raw %>%
    select(ATC1, ATC2, ATC3, ATC4, CMDECOD, CMTRT) %>%
    unique()

  var_labels(out) <- c(
    ATC1 = "ATC Class Level 1",
    ATC2 = "ATC Class Level 2",
    ATC3 = "ATC Class Level 3",
    ATC4 = "ATC Class Level 4",
    CMDECOD = "WHODrug Preferred Name",
    CMTRT = "Investigator-Specified\nTreatment Term"
  )

  result <- as_listing(
    out,
    key_cols = c("ATC1", "ATC2", "ATC3", "ATC4", "CMDECOD", "CMTRT"),
    disp_cols = names(out),
    main_title = "Listing of Concomitant Medication Class, Preferred Name, and Investigator-Specified Terms"
  ) %>% head(50)

  testthat::expect_snapshot(result)
})
