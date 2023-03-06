testthat::test_that("EXL01 listing is produced correctly", {
  out <- adex_raw %>%
    filter(PARAMCD == "DOSE" & !is.na(AVAL) & SAFFL == "Y") %>%
    mutate(
      CRTNPT = paste(SITEID, SUBJID, sep = "/")
    ) %>%
    arrange(CRTNPT, AVISIT) %>%
    select(CRTNPT, AVISIT, EXSTDY, EXENDY, TRT01A, AVAL, AVALU, EXDOSFRQ, EXROUTE)

  formatters::var_labels(out) <- c(
    CRTNPT = "Center/Subject ID",
    AVISIT = "Visit",
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
    key_cols = c("CRTNPT", "AVISIT"),
    disp_cols = names(out),
    main_title = "Listing of Exposure to Study Drug"
  ) %>% head(50)

  testthat::expect_snapshot(result)
})
