testthat::test_that("EGL01 listing is produced correctly", {
  get_param_unit_range <- function(dataset) {
    u_rng <- lapply(unique(dataset$PARAMCD), function(p) {
      dat <- dataset %>% filter(PARAMCD == p)
      list(
        unit = unique(dat$EGSTRESU),
        range = paste0(unique(dat$ANRLO), "-", unique(dat$ANRHI))
      )
    })
    names(u_rng) <- unique(dataset$PARAMCD)
    u_rng
  }

  eg_u_rng <- get_param_unit_range(adeg_pharmaverse)

  adeg_sub <- adeg_pharmaverse %>%
    filter(!is.na(AVAL) & SAFFL == "Y" & ANL01FL == "Y" & !is.na(ASEQ) & PARAMCD != "EGINTP" & DTYPE == "AVERAGE") %>%
    mutate(
      CRTNPT = paste(SITEID, sub("^.*-([[:alnum:]]+)$", "\\1", SUBJID), sep = "/"),
      AGSXRC = paste(AGE, SEX, RACE, sep = "/"),
      AVAL = format(round(AVAL, 2), nsmall = 2),
      AVAL_ANRIND = ifelse(ANRIND %in% c("NORMAL", ""), AVAL, paste(AVAL, substr(ANRIND, 1, 1), sep = "/")),
      CHG = format(round(CHG, 2), nsmall = 2)
    )

  anl_eg <- adeg_sub %>%
    select(SUBJID, CRTNPT, AGSXRC, TRT01A, PARAMCD, AVAL_ANRIND, CHG, ADY, AVISIT, ADTM) %>%
    tidyr::pivot_wider(
      id_cols = c(SUBJID, CRTNPT, AGSXRC, TRT01A, ADY, AVISIT, ADTM),
      names_from = PARAMCD,
      values_from = c(AVAL_ANRIND, CHG)
    )

  out <- anl_eg %>%
    select(
      CRTNPT, AGSXRC, TRT01A, AVISIT, ADY, AVAL_ANRIND_HR, CHG_HR, AVAL_ANRIND_QT, CHG_QT, AVAL_ANRIND_RR, CHG_RR
    ) %>%
    var_relabel(
      CRTNPT = "Center/Subject ID",
      AGSXRC = "Age/Sex/Race",
      TRT01A = "Treatment",
      AVISIT = "Visit",
      ADY = "Study\nDay",
      AVAL_ANRIND_HR = paste0("Heart Rate Result\n(", eg_u_rng$HR$unit[1], ");\nRange:(", eg_u_rng$HR$range[1], ")"),
      CHG_HR = "Heart Rate\nChange from BL",
      AVAL_ANRIND_QT = paste0("QT Duration Result\n(", eg_u_rng$QT$unit[1], ");\nRange:(", eg_u_rng$QT$range[1], ")"),
      CHG_QT = "QT Duration\nChange from BL",
      AVAL_ANRIND_RR = paste0("RR Duration Result\n(", eg_u_rng$RR$unit[1], ");\nRange:(", eg_u_rng$RR$range[1], ")"),
      CHG_RR = "RR Duration\nChange from BL"
    )

  result <- as_listing(
    out,
    key_cols = c("TRT01A", "CRTNPT", "AGSXRC", "AVISIT", "ADY"),
    disp_cols = names(out),
    main_title = "Listing of ECG Data: Safety-Evaluable Patients",
    main_footer = "Baseline is the patient's last observation prior to initiation of study drug. Abnormalities are
    flagged as high (H) or low (L) if outside the Roche standard reference range."
  ) %>% head(50)

  testthat::expect_snapshot(result)
})
