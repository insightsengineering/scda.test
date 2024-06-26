testthat::test_that("MHL01 listing is produced correctly", {
  out <- admh_raw %>%
    mutate(
      ASR = paste(AGE, SEX, RACE, sep = "/"),
      ID = paste(SITEID, SUBJID, sep = "/"),
      TRTSDTM = toupper(format(as.Date(TRTSDTM), "%d%b%Y")),
      ASTDTM = toupper(format(as.Date(ASTDTM), "%d%b%Y")),
      AENDTM = toupper(format(as.Date(AENDTM), "%d%b%Y"))
    ) %>%
    select(ID, ASR, TRT01A, MHBODSYS, MHDECOD, TRTSDTM, ASTDTM, ASTDY, AENDTM, AENDY, ATIREL)

  var_labels(out) <- c(
    ID = "Center/Patient ID",
    ASR = "Age/Sex/Race",
    TRT01A = "Treatment",
    MHBODSYS = "SOC",
    MHDECOD = "Disease Term",
    TRTSDTM = "Date of First\nStudy Drug\nAdministration",
    ASTDTM = "Start Date\nof Disease",
    ASTDY = "Start Day\nof Disease",
    AENDTM = "End Date\nof Disease",
    AENDY = "End Day\nof Disease",
    ATIREL = "Time Relation\nof Disease"
  )

  result <- as_listing(
    out,
    key_cols = c("TRT01A", "ID", "ASR", "MHBODSYS", "MHDECOD"),
    disp_cols = names(out),
    main_title = "Listing of Medical History and Concurrent Diseases"
  ) %>% head(50)

  testthat::expect_snapshot(result)
})
