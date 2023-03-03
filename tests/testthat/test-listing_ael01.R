testthat::test_that("AEL01 listing is produced correctly", {
  out <- adae_raw %>%
    select(AESOC, AEDECOD, AELLT, AETERM) %>%
    unique()

  formatters::var_labels(out) <- c(
    AESOC = "MedDRA System Organ Class",
    AEDECOD = "MedDRA Preferred Term",
    AELLT = "MedDRA Lowest Level Term",
    AETERM = "Investigator-Specified\nAdverse Event Term"
  )

  testthat::expect_message(result <- as_listing(
    out,
    key_cols = c("AESOC", "AEDECOD", "AELLT"),
    disp_cols = names(out),
    main_title = "Listing of Preferred Terms, Lowest Level Terms, and Investigator-Specified Adverse Event Terms"
  ), "sorting incoming data by key columns")

  testthat::expect_snapshot(result)
})
