testthat::test_that("PKPL02 listing is produced correctly", {
  drug_a <- "XANOMELINE"
  spec <- "URINE"
  adpp <- adpp_pharmaverse
  adpp_x <- adpp %>%
    mutate(REGIMEN = ifelse("REGIMEN" %in% names(adpc), REGIMEN, "BID")) %>%
    filter(
      PPCAT == "XANOMELINE",
      PPSPEC == spec
    )

  out <- adpp_x %>%
    mutate(PARAM = paste0(PPTEST, " (", AVALU, ")")) %>%
    select(TRT01A, USUBJID, AVISIT, PARAM, AVAL) %>%
    unique() %>%
    tidyr::pivot_wider(
      id_cols = c(TRT01A, USUBJID, AVISIT),
      names_from = PARAM,
      values_from = AVAL
    )

  var_labels(out) <- names(out)
  out <- out %>% var_relabel(
    TRT01A = "Treatment Group",
    USUBJID = "Subject ID",
    AVISIT = "Visit"
  )

  result <- as_listing(
    out,
    key_cols = c("TRT01A", "USUBJID", "AVISIT"),
    disp_cols = names(out),
    main_title = paste("Listing of", drug_a, spec, "PK Parameters, PK Population\nProtocol: xxnnnnn"),
    subtitles = paste("Analyte:", drug_a)
  ) %>% head(50)

  testthat::expect_snapshot(result)
})
