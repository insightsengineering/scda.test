testthat::test_that("PKCL02 listing is produced correctly", {
  drug_a <- "XANOMELINE"
  spec <- "URINE"
  adpc <- adpc_pharmaverse
  adpc_x <- adpc %>%
    mutate(REGIMEN = ifelse("REGIMEN" %in% names(adpc), REGIMEN, "BID")) %>%
    filter(
      TRT01A == "Xanomeline High Dose",
      PCTEST == drug_a,
      PARCAT1 == spec
    )

  out <- adpc_x %>%
    tidyr::pivot_longer(
      cols = c(AVAL),
      names_to = "URCD",
      values_to = "VALUE"
    ) %>%
    mutate(
      URCD = case_when(
        URCD == "AVAL" ~ "UR_Conc",
        TRUE ~ URCD
      ),
      UNIT = case_when(
        URCD == "UR_Conc" ~ as.character(AVALU),
        TRUE ~ "NA"
      )
    ) %>%
    mutate(
      PARAM_INT = paste0(
        URCD, " (", UNIT, ") -\nUrine Collection\nInterval",
        ifelse(PCTPT == "Predose", "", " (hours)"), ":\n",
        gsub("[PTH]", "", PCTPT)
      )
    ) %>%
    select(TRT01A, USUBJID, VISIT, PARAM_INT, VALUE) %>%
    unique() %>%
    tidyr::pivot_wider(
      id_cols = c(TRT01A, USUBJID, VISIT),
      names_from = PARAM_INT,
      values_from = VALUE
    )

  var_labels(out) <- names(out)
  out <- out %>% var_relabel(
    TRT01A = "Treatment Group",
    USUBJID = "Subject ID",
    VISIT = "Visit"
  )

  result <- as_listing(
    out,
    key_cols = c("TRT01A", "USUBJID", "VISIT"),
    disp_cols = names(out),
    main_title = paste0(
      "Listing of ", drug_a, " Urine Concentration and Volumes following ",
      unique(adpc_x$REGIMEN)[1], " of ", drug_a, ", PK Population\nProtocol: xxnnnnn"
    ),
    subtitles = paste("Analyte:", drug_a)
  ) %>% head(50)

  testthat::expect_snapshot(result)
})
