testthat::test_that("DSL02 listing is produced correctly", {
  out <- adsl_raw %>%
    filter(AEWITHFL == "Y") %>%
    mutate(
      ID = paste(SITEID, SUBJID, sep = "/"),
      ASR = paste(AGE, SEX, RACE, sep = "/"),
      DISCONT = ifelse(!is.na(DCSREAS) & EOSSTT != "COMPLETED", "Yes", "No"),
      SSADTM = as.POSIXct(
        strftime(TRTSDTM, format = "%Y-%m-%d %H:%M:%S"),
        format = "%Y-%m-%d",
        tz = "UTC"
      ),
      SSAEDY = as.numeric(ceiling(difftime(EOSDT, SSADTM, units = "days"))),
      RANDEDY = as.numeric(ceiling(difftime(EOSDT, RANDDT, units = "days"))),
    ) %>%
    filter(DISCONT == "Yes") %>%
    select(ID, ASR, TRT01A, SSADTM, EOSDY, SSAEDY, RANDEDY, DCSREAS)

  formatters::var_labels(out) <- c(
    ID = "Center/Patient ID",
    ASR = "Age/Sex/Race",
    TRT01A = "Treatment",
    SSADTM = "Date of First\nStudy Drug\nAdministration",
    EOSDY = "Day of Last\nStudy Drug\nAdministration",
    SSAEDY = "Day of Study\nDiscontinuation\nRelative to First\nStudy Drug\nAdministration",
    RANDEDY = "Day of Study\nDiscontinuation\nRelative to\nRandomization",
    DCSREAS = "Reason for\nDiscontinuation"
  )

  testthat::expect_message(result <- as_listing(
    out,
    disp_cols = names(out),
    main_title = "Listing of Patients Who Discontinued Early from Study"
  ), "sorting incoming data by key columns")

  testthat::expect_snapshot(result)
})
