testthat::test_that("DSL01 listing is produced correctly", {
  out <- adsl_raw %>%
    mutate(
      ID = paste(SITEID, SUBJID, sep = "/"),
      ASR = paste(AGE, SEX, RACE, sep = "/"),
      SSADM = toupper(format(as.Date(TRTSDTM), format = "%d%b%Y")),
      STDWD = as.numeric(ceiling(difftime(TRTEDTM, TRTSDTM, units = "days"))),
      DISCONT = ifelse(!is.na(DCSREAS) & toupper(EOSSTT) == "DISCONTINUED", "Yes", "No")
    ) %>%
    select(ID, ASR, ARMCD, SSADM, STDWD, DISCONT)

  formatters::var_labels(out) <- c(
    ID = "Center/Patient ID",
    ASR = "Age/Sex/Race",
    ARMCD = "Treatment",
    SSADM = "Date of First\nStudy Drug\nAdministration",
    STDWD = "Study Day\nof Withdrawal",
    DISCONT = "Discontinued\nEarly from Study?"
  )

  testthat::expect_message(result <- as_listing(
    out,
    disp_cols = names(out),
    main_title = "Listing of Patients with Study Drug Withdrawn Due to Adverse Events",
    subtitles = "Population: All Patients",
    main_footer = c("Program: xxxx", "Output: xxxx"),
    prov_footer = "Page 1 of 1"
  ), "sorting incoming data by key columns")

  testthat::expect_snapshot(result)
})
