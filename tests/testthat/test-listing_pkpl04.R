testthat::test_that("PKPL04 listing is produced correctly", {
  adpp <- adpp_raw
  visit <- "CYCLE 1 DAY 1"
  drug_a <- "Plasma Drug X"
  drug_b <- "Plasma Drug Y"

  adpp_x <- adpp %>%
    filter(
      AVISIT == visit,
      PPCAT %in% c(drug_a, drug_b),
      PARAMCD %in% c("CMAX", "AUCIFO")
    ) %>%
    mutate(
      PARAM_U = paste0(PARAM, " (", AVALU, ")")
    )

  adpp_ratio <- adpp_x %>%
    tidyr::pivot_wider(
      id_cols = c(USUBJID, PARAM_U),
      names_from = PPCAT,
      values_from = AVAL
    )

  adpp_ratio[, paste0(drug_a, "/", drug_b)] <- as.numeric(unlist(adpp_ratio[, drug_a] / adpp_ratio[, drug_b]))

  out <- adpp_ratio %>%
    tidyr::pivot_wider(
      id_cols = USUBJID,
      names_from = PARAM_U,
      names_glue = "{PARAM_U}\n{.value}",
      values_from = c(all_of(drug_a), all_of(drug_b), paste0(drug_a, "/", drug_b))
    ) %>%
    select(names(.)[c(1:2, 4, 6, 3, 5, 7)])

  formatters::var_labels(out) <- names(out)
  out <- out %>% formatters::var_relabel(USUBJID = "Subject ID")

  testthat::expect_message(result <- as_listing(
    out,
    key_cols = "USUBJID",
    disp_cols = names(out),
    main_title = paste0(
      "Listing of Individual ", drug_a, " ", paste(unique(adpp_x$PARAM), collapse = " and "), " Ratios following ",
      unique(adpp_x$REGIMEN), " ", paste(drug_a, drug_b, sep = " or "), ", PK Population\nProtocol: xxnnnnn",
      "\nVisit: ", unique(adpp_x$AVISIT)
    ),
    subtitles = paste0("\nAnalytes: ", paste(drug_a, drug_b, sep = " and "))
  ) %>% head(50), "sorting incoming data by key columns")

  testthat::expect_snapshot(result)
})