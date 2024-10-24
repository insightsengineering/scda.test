testthat::test_that("EXL01 listing is produced correctly", {
  out <- adex_pharmaverse %>%
    filter(PARAMCD == "DOSE" & !is.na(AVAL) & SAFFL == "Y") %>%
    mutate(
      CRTNPT = paste(SITEID, SUBJID, sep = "/")
    ) %>%
    arrange(CRTNPT, VISIT) %>%
    select(CRTNPT, VISIT, EXSTDY, EXENDY, TRT01A, AVAL, AVALU, EXDOSFRQ, EXROUTE)

  var_labels(out) <- c(
    CRTNPT = "Center/Subject ID",
    VISIT = "Visit",
    EXSTDY = "Study Day\nFrom",
    EXENDY = "Study Day\nTo",
    TRT01A = "Treatment",
    AVAL = "Dose",
    AVALU = "Unit",
    EXDOSFRQ = "Frequency",
    EXROUTE = "Route"
  )

  result <- as_listing(
    out,
    key_cols = c("TRT01A", "CRTNPT", "VISIT"),
    disp_cols = names(out),
    main_title = "Listing of Exposure to Study Drug"
  ) %>% head(50)

  testthat::expect_snapshot(result)
})
