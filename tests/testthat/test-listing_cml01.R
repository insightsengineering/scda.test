testthat::test_that("CML01 listing is produced correctly", {
  out <- adcm_raw %>%
    filter(!is.na(CMDECOD)) %>%
    mutate(
      ID = paste(SITEID, SUBJID, sep = "/"),
      AGSXRC = paste(AGE, SEX, RACE, sep = "/"),
      TRTSD = toupper(format(as.Date(TRTSDTM), "%d%b%Y")),
      CMASTD = toupper(format(as.Date(ASTDTM), "%d%b%Y")),
      CMSTRFL = ifelse(ASTDY < 0, "Yes", "No"),
      CMENRFL = ifelse(CMENRTPT == "ONGOING", "Yes", "No")
    ) %>%
    select(
      ID, AGSXRC, TRT01A, CMDECOD, TRTSD, CMASTD, ASTDY, ADURN,
      CMSTRFL, CMENRFL, CMDOSE, CMDOSU, CMDOSFRQ, CMROUTE
    )

  var_labels(out) <- c(
    ID = "Center/Patient ID",
    AGSXRC = "Age/Sex/Race",
    TRT01A = "Treatment",
    CMDECOD = "Medication Name",
    TRTSD = "Date of First\nStudy Drug\nAdministration",
    CMASTD = "Medication\nStart Date",
    ASTDY = "Study Day",
    ADURN = "Duration\n(days)",
    CMSTRFL = "Previous?",
    CMENRFL = "Ongoing\nat Final\nContact?",
    CMDOSE = "Dose",
    CMDOSU = "Dose Unit",
    CMDOSFRQ = "Frequency",
    CMROUTE = "Route"
  )

  testthat::expect_message(result <- as_listing(
    out,
    key_cols = c("TRT01A", "ID", "AGSXRC", "CMDECOD"),
    disp_cols = names(out),
    main_title = "Listing of Previous and Concomitant Medications"
  ) %>% head(50), "sorting incoming data by key columns")

  testthat::expect_snapshot(result)
})
