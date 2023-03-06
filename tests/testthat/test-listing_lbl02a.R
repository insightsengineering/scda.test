testthat::test_that("LBL02A listing is produced correctly", {
  adlb_x <- adlb_raw %>%
    filter(ANRIND %in% c("LOW", "HIGH"), !ATOXGR %in% c("0", ""), LBTEST != "") %>%
    mutate(
      LBTEST_U = paste0(LBTEST, " (", AVALU, ")"),
      CPID = paste(SITEID, SUBJID, sep = "/"),
      ADTM = toupper(format(as.Date(ADTM), "%d%b%Y")),
      AVAL = format(round(AVAL, 1), nsmall = 1),
      LBNRNG = paste(format(round(ANRLO, 1), nsmall = 1), format(round(ANRHI, 1), nsmall = 1), sep = " - "),
      ANRIND_GR = factor(
        case_when(
          ANRIND == "LOW" ~ paste0("L", sub("-", "", ATOXGR)),
          ANRIND == "HIGH" ~ paste0("H", ATOXGR)
        )
      )
    ) %>%
    select(LBTEST_U, TRT01A, CPID, ADY, ADTM, AVAL, ADY, LBNRNG, ANRIND_GR) %>%
    unique() %>%
    arrange(CPID, ADY) %>%
    group_by(LBTEST_U, CPID) %>%
    mutate(DLD = ADY - lag(ADY)) %>%
    ungroup() %>%
    mutate(DLD = ifelse(is.na(DLD), 0, DLD))

  out <- adlb_x %>%
    select(LBTEST_U, TRT01A, CPID, ADY, ADTM, DLD, AVAL, LBNRNG, ANRIND_GR)

  formatters::var_labels(out) <- names(out)
  out <- out %>% formatters::var_relabel(
    LBTEST_U = "Lab Test (Unit)",
    TRT01A = "Treatment",
    CPID = "Center/Patient ID",
    ADY = "Study\nDay",
    ADTM = "Date",
    DLD = "Days Since\nLast Dose of\nStudy Drug",
    AVAL = "Result",
    LBNRNG = "Lab Normal\nRange",
    ANRIND_GR = "NCI\nCTCAE\nGrade"
  )

  testthat::expect_message(result <- as_listing(
    out,
    key_cols = c("LBTEST_U", "TRT01A", "CPID"),
    disp_cols = names(out),
    main_title = "Listing of Laboratory Abnormalities Defined by NCI CTCAE Grade >= 1",
    main_footer = "NCI CTCAE grade is displayed as abnormal high (H) or low (L) followed by the grade."
  ) %>% head(50), "sorting incoming data by key columns")

  testthat::expect_snapshot(result)
})
