testthat::test_that("AEL04 listing is produced correctly", {
  out <- adsl_raw %>%
    filter(!is.na(DTHADY)) %>%
    mutate(
      ID = paste(SITEID, SUBJID, sep = "/"),
      AGSXRC = paste(AGE, SEX, RACE, sep = "/"),
      TRTSD = toupper(format(as.Date(TRTSDTM), "%d%b%Y"))
    ) %>%
    arrange(SUBJID) %>%
    select(ID, AGSXRC, TRT01A, TRTSD, EOSDY, DTHADY, DTHCAUS, ADTHAUT)

  formatters::var_labels(out) <- c(
    ID = "Center/Patient ID",
    AGSXRC = "Age/Sex/Race",
    TRT01A = "Treatment",
    TRTSD = "Date of First\nStudy Drug\nAdministration",
    EOSDY = "Day of Last\nStudy Drug\nAdministration",
    DTHADY = "Day of\nDeath",
    DTHCAUS = "Cause of Death",
    ADTHAUT = "Autopsy\nPerformed?"
  )

  testthat::expect_message(result <- as_listing(
    out,
    key_cols = c("TRT01A", "ID"),
    disp_cols = names(out),
    main_title = "Listing of Patient Deaths"
  ) %>% head(50), "sorting incoming data by key columns")

  testthat::expect_snapshot(result)
})
