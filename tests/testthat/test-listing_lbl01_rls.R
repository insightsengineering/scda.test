testthat::test_that("LBL01_RLS listing is produced correctly", {
  adlb_x <- adlb_raw %>%
    mutate(ANRIND = factor(
      case_when(
        ANRIND == "LOW" & AVAL > stats::quantile(adlb_raw$AVAL, probs = c(0.1)) ~ "L",
        ANRIND == "HIGH" & AVAL < stats::quantile(adlb_raw$AVAL, probs = c(0.9)) ~ "H",
        ANRIND == "LOW" & AVAL <= stats::quantile(adlb_raw$AVAL, probs = c(0.1)) ~ "LL",
        ANRIND == "HIGH" & AVAL >= stats::quantile(adlb_raw$AVAL, probs = c(0.9)) ~ "HH",
        TRUE ~ as.character(ANRIND)
      ),
      levels = c("", "H", "HH", "L", "LL", "NORMAL")
    ))

  adlb_x <- adlb_x %>%
    filter(!is.na(LBSEQ) & !is.na(ADY) & ANRIND != "") %>%
    mutate(
      CPID = paste(SITEID, SUBJID, sep = "/"),
      PARAM_U = paste0(PARAMCD, "\n(", AVALU, ")"),
      AVALC = as.character(format(round(AVAL, 2), nsmall = 2))
    ) %>%
    mutate(AVAL_GR = ifelse(
      !ANRIND %in% c("H", "HH", "L", "LL"), AVALC,
      paste(AVALC, ANRIND, sep = " / ")
    )) %>%
    select(CPID, TRT01A, ADY, LBSEQ, PARAM_U, AVAL_GR) %>%
    unique()

  out <- adlb_x %>%
    arrange(CPID, ADY, PARAM_U, LBSEQ) %>%
    tidyr::pivot_wider(
      id_cols = c(TRT01A, CPID, ADY, LBSEQ),
      names_from = PARAM_U,
      values_from = AVAL_GR
    ) %>%
    group_by(CPID) %>%
    mutate(DLD = ADY - lag(ADY)) %>%
    ungroup() %>%
    select(CPID, TRT01A, ADY, DLD, unique(adlb_x$PARAM_U))

  formatters::var_labels(out) <- names(out)
  out <- out %>% formatters::var_relabel(
    TRT01A = "Treatment",
    CPID = "Center/Patient ID",
    ADY = "Study\nDay",
    DLD = "Days Since\nLast Dose of\nStudy Drug"
  )

  result <- as_listing(
    out,
    key_cols = c("CPID", "TRT01A"),
    disp_cols = names(out),
    main_title = "Listing of Laboratory Test Results Using Roche Safety Lab Standardization",
    main_footer = "Abnormalities are flagged as high (H) or low (L) if outside the Roche standard reference range;
    high high (HH) or low low (LL) if outside the Roche marked reference range with a clinically relevant change
    from baseline."
  ) %>% head(50)

  testthat::expect_snapshot(result)
})
