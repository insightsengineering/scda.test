testthat::test_that("PKPL01 listing is produced correctly", {
  drug_a <- "Xanomeline Low Dose"
  spec <- "PLASMA"
  adpp <- adpp_pharmaverse
  adpp_x <- adpp %>% filter(
    ARM == drug_a,
    PPSPEC == spec
  )

  # tmp solution
  adpp_x$AVISIT <- rep(c("visit 1", "visit 2", "visit 3"))

  out <- adpp_x %>%
    mutate(PARAM = paste0(PARAMCD, " (", PPORRESU, ")")) %>%
    select(TRT01A, USUBJID, AVISIT, PARAM, AVAL) %>%
    unique() %>% # This is a temp fix, as avisit was added this way
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
