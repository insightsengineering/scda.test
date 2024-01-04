testthat::test_that("LBL02B listing is produced correctly", {
  adlb_x <- adlb_raw %>%
    filter(
      !sub("-", "", ATOXGR) %in% c("", "0", "1"),
      LBTEST != "",
      !is.na(ADY),
      !ANRIND %in% c("", "NORMAL")
    ) %>%
    mutate(
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
    select(LBTEST, TRT01A, CPID, ADY, ADTM, AVAL, AVALU, LBNRNG, ANRIND_GR) %>%
    unique() %>%
    arrange(CPID, ADY) %>%
    group_by(LBTEST, CPID) %>%
    mutate(DLD = ADY - lag(ADY)) %>%
    ungroup() %>%
    mutate(DLD = ifelse(is.na(DLD), 0, DLD))

  out <- adlb_x %>%
    select(LBTEST, TRT01A, CPID, ADY, ADTM, DLD, AVAL, AVALU, LBNRNG, ANRIND_GR)

  formatters::var_labels(out) <- names(out)
  out <- out %>%
    formatters::var_relabel(
      LBTEST = "Lab Test",
      TRT01A = "Treatment",
      CPID = "Center/Patient ID",
      ADY = "Study\nDay",
      ADTM = "Date",
      DLD = "Days Since\nLast Dose of\nStudy Drug",
      AVAL = "Result",
      AVALU = "Unit",
      LBNRNG = "Lab Normal\nRange",
      ANRIND_GR = "NCI\nCTCAE\nGrade"
    )

  testthat::expect_message(result <- as_listing(
    out,
    key_cols = c("TRT01A", "LBTEST", "CPID"),
    disp_cols = names(out),
    main_title = "Listing of Laboratory Abnormalities Defined by NCI CTCAE Grade >= 2",
    main_footer = "NCI CTCAE grade is displayed as abnormal high (H) or low (L) followed by the grade."
  ) %>% head(50), "sorting incoming data by key columns")

  testthat::expect_snapshot(result)
})
