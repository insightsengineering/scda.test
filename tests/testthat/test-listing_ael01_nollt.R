testthat::test_that("AEL01_NOLLT listing is produced correctly", {
  out <- adae_raw %>%
    select(AESOC, AEDECOD, AETERM) %>%
    unique()

  formatters::var_labels(out) <- c(
    AESOC = "MedDRA System Organ Class",
    AEDECOD = "MedDRA Preferred Term",
    AETERM = "Investigator-Specified\nAdverse Event Term"
  )

  testthat::expect_message(result <- as_listing(
    out,
    key_cols = c("AESOC", "AEDECOD"),
    disp_cols = names(out),
    main_title = "Listing of Preferred Terms and Investigator-Specified Adverse Event Terms"
  ), "sorting incoming data by key columns")

  testthat::expect_snapshot(result)
})
