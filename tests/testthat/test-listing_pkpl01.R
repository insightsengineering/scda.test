testthat::test_that("PKPL01 listing is produced correctly", {
  drug_a <- "Plasma Drug X"
  spec <- "Plasma"
  adpp <- adpp_raw
  adpp_x <- adpp %>% filter(
    PPCAT == drug_a,
    PPSPEC == spec
  )

  out <- adpp_x %>%
    mutate(PARAM = paste0(PARAM, " (", AVALU, ")")) %>%
    select(TRT01A, USUBJID, AVISIT, PARAM, AVAL) %>%
    tidyr::pivot_wider(
      id_cols = c(TRT01A, USUBJID, AVISIT),
      names_from = PARAM,
      values_from = AVAL
    )

  formatters::var_labels(out) <- names(out)
  out <- out %>% formatters::var_relabel(
    TRT01A = "Treatment Group",
    USUBJID = "Subject ID",
    AVISIT = "Visit"
  )

  testthat::expect_message(result <- as_listing(
    out,
    key_cols = c("TRT01A", "USUBJID", "AVISIT"),
    disp_cols = names(out),
    main_title = paste("Listing of", drug_a, spec, "PK Parameters, PK Population\nProtocol: xxnnnnn"),
    subtitles = paste("Analyte:", drug_a)
  ) %>% head(50), "sorting incoming data by key columns")

  testthat::expect_snapshot(result)
})
