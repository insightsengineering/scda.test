testthat::test_that("ONCL01 listing is produced correctly", {
  out_ex <- adex_raw %>%
    filter(PARAMCD == "TNDOSE") %>%
    mutate(ID = paste(SITEID, SUBJID, sep = "/")) %>%
    select(ID, AVAL)

  out_rs <- adrs_raw %>%
    filter(
      PARAM %in% c("Investigator End Of Induction Response", "Best Confirmed Overall Response by Investigator")
    ) %>%
    mutate(ID = paste(SITEID, SUBJID, sep = "/")) %>%
    select(ID, AVALC, PARAM) %>%
    tidyr::pivot_wider(
      id_cols = ID,
      names_from = PARAM,
      values_from = AVALC
    ) %>%
    right_join(out_ex, ., by = "ID", multiple = "all")

  adtte_flt <- adtte_raw %>%
    filter(PARAMCD %in% c("OS", "PFS", "CRSD")) %>%
    mutate(
      PARAM = paste(PARAM, paste0("(", AVALU, ")"), sep = " "), ID = paste(SITEID, SUBJID, sep = "/"),
      trigeventpfs = ifelse(CNSR == 0, EVNTDESC, NA)
    )

  out_trg <- adtte_flt %>% select(ID, trigeventpfs)

  out_tte <- adtte_flt %>%
    select(ID, TRT01A, PARAM, AVAL, trigeventpfs) %>%
    tidyr::pivot_wider(
      id_cols = c(ID, TRT01A),
      names_from = PARAM,
      values_from = AVAL
    ) %>%
    mutate_at(
      c("Overall Survival (DAYS)", "Progression Free Survival (DAYS)", "Duration of Confirmed Response (DAYS)"),
      function(x) format(round(x, 1), nsmall = 1)
    ) %>%
    select(
      ID, TRT01A, `Overall Survival (DAYS)`, `Progression Free Survival (DAYS)`, `Duration of Confirmed Response (DAYS)`
    ) %>%
    right_join(out_trg, ., by = "ID", multiple = "all")

  out <- out_tte %>%
    right_join(out_rs, ., by = "ID", multiple = "all") %>%
    select(
      "ID", "TRT01A", "AVAL", "Best Confirmed Overall Response by Investigator",
      "Investigator End Of Induction Response", "Overall Survival (DAYS)",
      "Progression Free Survival (DAYS)", "trigeventpfs", "Duration of Confirmed Response (DAYS)"
    )

  result <- as_listing(
    out,
    key_cols = c("TRT01A", "ID"),
    disp_cols = names(out),
    main_title = "Listing of Individual Efficacy Data",
    main_footer = "PFS = Progression Free Survival"
  ) %>% head(50)

  testthat::expect_snapshot(result)
})
