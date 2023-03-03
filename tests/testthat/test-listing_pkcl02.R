testthat::test_that("PKCL02 listing is produced correctly", {
  drug_a <- "Drug X"
  spec <- "URINE"
  adpc <- adpc_raw
  adpc_x <- adpc %>%
    mutate(REGIMEN = ifelse("REGIMEN" %in% names(adpc), REGIMEN, "BID")) %>%
    filter(
      grepl(drug_a, PARAM),
      ASMED == spec
    )

  out <- adpc_x %>%
    tidyr::pivot_longer(
      cols = c(AVAL, PCVOL),
      names_to = "URCD",
      values_to = "VALUE"
    ) %>%
    mutate(
      URCD = case_when(
        URCD == "AVAL" ~ "UR_Conc",
        URCD == "PCVOL" ~ "Vurine",
        TRUE ~ URCD
      ),
      UNIT = case_when(
        URCD == "UR_Conc" ~ as.character(AVALU),
        URCD == "Vurine" ~ as.character(PCVOLU),
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

  formatters::var_labels(out) <- names(out)
  out <- out %>% formatters::var_relabel(
    TRT01A = "Treatment Group",
    USUBJID = "Subject ID",
    VISIT = "Visit"
  )

  testthat::expect_message(result <- as_listing(
    out,
    key_cols = c("TRT01A", "USUBJID", "VISIT"),
    disp_cols = names(out),
    main_title = paste0("Listing of ", drug_a, " Urine Concentration and Volumes following ",
                        unique(adpc_x$REGIMEN)[1], " of ", drug_a, ", PK Population\nProtocol: xxnnnnn"),
    subtitles = paste("Analyte:", drug_a)
  ), "sorting incoming data by key columns")

  testthat::expect_snapshot(result)
})
