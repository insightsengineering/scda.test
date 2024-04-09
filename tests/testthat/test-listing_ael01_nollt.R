testthat::test_that("AEL01_NOLLT listing is produced correctly", {
  out <- adae_raw %>%
    select(AESOC, AEDECOD, AETERM) %>%
    unique()

  var_labels(out) <- c(
    AESOC = "MedDRA System Organ Class",
    AEDECOD = "MedDRA Preferred Term",
    AETERM = "Investigator-Specified\nAdverse Event Term"
  )

  result <- as_listing(
    out,
    key_cols = c("AESOC", "AEDECOD"),
    disp_cols = names(out),
    default_formatting = list(
      all = fmt_config(align = "left"),
      numeric = fmt_config(align = "center")
    ),
    main_title = "Listing of Preferred Terms and Investigator-Specified Adverse Event Terms"
  ) %>% head(50)

  testthat::expect_snapshot(result)
})
