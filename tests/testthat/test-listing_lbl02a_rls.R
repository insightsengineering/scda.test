testthat::test_that("LBL02A_RLS listing is produced correctly", {
  adlb_x <- adlb_pharmaverse %>%
    filter(
      LBTEST != ""
    ) %>%
    mutate(
      LBTEST_U = paste0(LBTEST, " (", AVALU, ")"),
      CPID = paste(SITEID, SUBJID, sep = "/")
    ) %>%
    mutate(CRC = paste("40%", "40%", sep = " / "))

  std_rng <- adlb_x %>%
    group_by(LBTEST_U) %>%
    summarise(
      STD_RNG_LO = stats::quantile(AVAL, probs = c(0.1), na.rm = TRUE),
      STD_RNG_HI = stats::quantile(AVAL, probs = c(0.9), na.rm = TRUE)
    ) %>%
    ungroup()

  adlb_x <- adlb_x %>%
    left_join(std_rng, by = "LBTEST_U") %>%
    mutate(
      AVAL = format(round(AVAL, 1), nsmall = 1),
      PCHG = format(round(PCHG, 1), nsmall = 1),
      LBNRNG = paste(ANRLO, ANRHI, sep = " - "),
      STD_RNG_LO = format(round(STD_RNG_LO, 1), nsmall = 1),
      STD_RNG_HI = format(round(STD_RNG_HI, 1), nsmall = 1)
    ) %>%
    mutate(
      STD_RNG = paste(STD_RNG_LO, STD_RNG_HI, sep = " - "),
      ANRIND = factor(case_when(
        ANRIND == "LOW" & AVAL > STD_RNG_LO ~ "L",
        ANRIND == "HIGH" & AVAL < STD_RNG_HI ~ "H",
        ANRIND == "LOW" & AVAL <= STD_RNG_LO ~ "LL",
        ANRIND == "HIGH" & AVAL >= STD_RNG_HI ~ "HH",
        TRUE ~ ""
      ))
    ) %>%
    select(LBTEST_U, TRT01A, CPID, ADY, AVAL, PCHG, STD_RNG, LBNRNG, CRC, ANRIND) %>%
    unique() %>%
    arrange(CPID, ADY) %>%
    group_by(LBTEST_U, CPID) %>%
    mutate(DLD = ADY - lag(ADY)) %>%
    ungroup() %>%
    mutate(DLD = ifelse(is.na(DLD), 0, DLD))

  out <- adlb_x %>%
    select(LBTEST_U, TRT01A, CPID, ADY, DLD, AVAL, PCHG, STD_RNG, LBNRNG, CRC, ANRIND)

  var_labels(out) <- names(out)
  out <- out %>%
    var_relabel(
      LBTEST_U = "Lab Test (Unit)",
      TRT01A = "Treatment",
      CPID = "Center/Patient ID",
      ADY = "Study\nDay",
      DLD = "Days Since\nLast Dose of\nStudy Drug",
      AVAL = "Result",
      PCHG = "% Change\nfrom\nBaseline",
      STD_RNG = "Standard\nReference\nRange",
      LBNRNG = "Marked\nReference\nRange",
      CRC = "Clinically\nRelevant\nChange\nDec./Inc.",
      ANRIND = "Abnormality\nFlag"
    )

  result <- as_listing(
    out,
    key_cols = c("TRT01A", "LBTEST_U", "CPID"),
    disp_cols = names(out),
    main_title = "Listing of Laboratory Abnormalities Defined by Roche Safety Lab Standardization",
    main_footer = paste(
      "Standard reference range, marked reference range and clinically relevant change from baseline are from the",
      "Roche Safety Lab Standardization guideline.  Abnormalities are flagged as high (H) or low (L) if outside",
      "the standard reference range; high high (HH) or low low (LL) if outside the marked reference range with a",
      "clinically relevant change from baseline."
    )
  ) %>% head(50)

  testthat::expect_snapshot(result)
})
