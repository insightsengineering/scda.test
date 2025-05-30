testthat::test_that("ADAL02 listing is produced correctly", {
  adab <- adab_raw %>%
    filter(NFRLT %% 1 == 0 & NFRLT > 0)

  trt <- "A: Drug X"
  drug_a <- "A: Drug X Antibody"
  drugcd <- unique(adab$PARAMCD[adab$PARAM == "Antibody titer units"])[1]
  min_titer <- 1.10

  adab_x <- adab %>%
    filter(
      ARM == trt,
      PARCAT1 == drug_a,
      ADPBLPFL == "Y"
    ) %>%
    select(-PARAMCD, -AVALC, -AVALU, -ARRLT, -NRRLT) %>%
    unique() %>%
    tidyr::pivot_wider(
      names_from = PARAM,
      values_from = AVAL
    ) %>%
    filter(if_any(matches("Treatment Emergent - Positive"), ~ .x == 1)) %>%
    mutate(
      VISN = factor(paste0(
        VISIT, "\n(Day ",
        ifelse(
          NFRLT %% 1 == 0,
          NFRLT,
          as.character(format(round(NFRLT, 2), nsmall = 2))
        ),
        ")"
      )),
      PTES = ifelse(
        ifelse("Treatment induced ADA" %in% names(.), `Treatment induced ADA` == 1, FALSE),
        ifelse(
          "Transient ADA" %in% names(.) & `Transient ADA` == 1,
          "Induced (Transient)",
          "Induced (Persistent)"
        ),
        "Enhanced"
      )
    ) %>%
    mutate(
      AVAL = paste0(
        ifelse(
          ifelse("ADA interpreted per sample result" %in% names(.), `ADA interpreted per sample result` == 0, FALSE),
          "NEGATIVE",
          ifelse(
            ifelse("Antibody titer units" %in% names(.), !is.na(`Antibody titer units`), FALSE),
            ifelse(
              `Antibody titer units` < min_titer,
              paste0("<", format(min_titer, nsmall = 2)),
              as.character(format(round(`Antibody titer units`, 2), nsmall = 2))
            ),
            "---"
          )
        ),
        ifelse(
          ifelse("NAB interpreted per sample result" %in% names(.), `NAB interpreted per sample result` == 1, FALSE),
          "*",
          ""
        )
      )
    )

  out <- adab_x %>%
    select(USUBJID, VISN, AVAL, PTES) %>%
    tidyr::pivot_wider(
      names_from = VISN,
      values_from = AVAL
    ) %>%
    select(USUBJID, unique(adab_x$VISN[order(adab_x$NFRLT)]), PTES)

  var_labels(out) <- names(out)

  out <- out %>%
    var_relabel(
      USUBJID = "Subject ID",
      PTES = "Patient Treatment\nEmergent ADA Status"
    )

  result <- as_listing(
    out,
    key_cols = "USUBJID",
    disp_cols = names(out),
    main_title = paste0(
      "Listing of Anti-", drugcd, " Antibody Data for Treatment Emergent ADA Positive Patients, PK Population",
      "\nProtocol: ", unique(adab$PARCAT1)[1]
    ),
    subtitles = paste("\nTreatment Group:", trt),
    main_footer = "Minimum reportable titer = 1.10 (example only)
--- = No sample evaluated
ADA = Anti-Drug Antibodies (is also referred to as ATA, or Anti-Therapeutic Antibodies)
Number of patients positive for Treatment Emergent ADA = the number of post-baseline evaluable patients determined to
have treatment-induced ADA or treatment-enhanced ADA during the study period. Treatment-induced ADA = a patient with
negative or missing baseline ADA result(s) and at least one positive post-baseline ADA result.
Treatment-enhanced ADA = a patient with positive ADA result at baseline who has one or more post-baseline titer
results that are at least 0.60 t.u. greater than the baseline titer result.
Transient ADA = ADA positive result detected (a) at only one post-baseline sampling timepoint (excluding last
timepoint) OR (b) at 2 or more timepoints during treatment where the first and last ADA positive samples  are
separated by a period of < 16 weeks, irrespective of any negative samples in between.
Persistent ADA =  ADA positive result detected (a) at the last post-baseline sampling timepoint, OR (b) at 2 or more
time points during treatment where the first and last ADA positive samples are separated by a period ≥ 16 weeks,
irrespective of any negative samples in between.
Asterisk denotes sample that tested positive for Neutralizing Antibodies."
  ) %>% head(50)

  testthat::expect_snapshot(result)
})
