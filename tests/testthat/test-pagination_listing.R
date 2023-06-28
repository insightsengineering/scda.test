# Using ael03 with minor modifications to test pagination for listings

# Data pre-processing
result <- adae_raw %>%
  filter(AESER == "Y") %>%
  mutate(
    CPID = paste(SITEID, SUBJID, sep = "/"),
    ASR = paste(AGE, SEX, RACE, sep = "/"),
    Date_First = toupper(format(as.Date(TRTSDTM), "%d%b%Y")),
    Duration = AENDY - ASTDY + 1,
    Related = ifelse(AEREL == "Y", "Yes", ifelse(AEREL == "N", "No", "")),
    Outcome = case_when(
      AEOUT == "FATAL" ~ 1,
      AEOUT == "NOT RECOVERED/NOT RESOLVED" ~ 2,
      AEOUT == "RECOVERED/RESOLVED" ~ 3,
      AEOUT == "RECOVERED/RESOLVED WITH SEQUELAE" ~ 4,
      AEOUT == "RECOVERING/RESOLVING" ~ 5,
      AEOUT == "UNKNOWN" ~ 6
    ),
    Treated = ifelse(AECONTRT == "Y", "Yes", ifelse(AECONTRT == "N", "No", "")),
    Action = case_when(
      AEACN == "DOSE INCREASED" ~ 1,
      AEACN == "DOSE NOT CHANGED" ~ 2,
      AEACN == "DOSE REDUCED" | AEACN == "DOSE RATE REDUCED" ~ 3,
      AEACN == "DRUG INTERRUPTED" ~ 4,
      AEACN == "DRUG WITHDRAWN" ~ 5,
      AEACN == "NOT APPLICABLE" | AEACN == "NOT EVALUABLE" ~ 6,
      AEACN == "UNKNOWN" ~ 7
    ),
    SERREAS = case_when(
      AESDTH == "Y" ~ "1",
      AESLIFE == "Y" ~ "2",
      AESHOSP == "Y" ~ "3",
      AESDISAB == "Y" ~ "4",
      AESCONG == "Y" ~ "5",
      AESMIE == "Y" ~ "6",
      TRUE ~ " "
    )
  ) %>%
  select(CPID, ASR, TRT01A, AEDECOD, Date_First, ASTDY, Duration, AESEV, Related, Outcome, Treated, Action, SERREAS)

# Adding labels
formatters::var_labels(result) <- c(
  CPID = "Center/Patient ID",
  ASR = "Age/Sex/Race",
  TRT01A = "Treatment",
  AEDECOD = "Adverse\nEvent MedDRA\nPreferred Term",
  Date_First = "Date of\nFirst Study\nDrug\nAdministration",
  ASTDY = "Study\nDay of\nOnset",
  Duration = "AE\nDuration\nin Days",
  AESEV = "Most\nExtreme\nIntensity",
  Related = "Caused by\nStudy\nDrug",
  Outcome = "Outcome\n(1)",
  Treated = "Treatment\nfor AE",
  Action = "Action\nTaken\n(2)",
  SERREAS = "Reason\nClassified\nas Serious\n(3)"
)

# Creating the list
lst_res <- as_listing(
  result,
  key_cols = c("CPID", "ASR", "TRT01A"),
  disp_cols = names(result),
  main_title = "Listing of Serious Adverse Events",
  subtitles = c("Some long subtitles that should eventually be split if wrapping is there", "maybe"),
  main_footer = "(1) Outcome: 1 = fatal; 2 = not recovered/not resolved; 3 = recovered/resolved;
    4 = recovered/resolved with sequelae; 5 = recovering/resolving; 6 = unknown.
(2) Action taken with study drug: 1 = dose increased; 2 = dose not changed; 3 = dose reduced; 4 = drug interrupted;
    5 = drug withdrawn; 6 = not applicable; 7 = unknown.
(3) Reason classified as serious: 1 = resulted in death; 2 = life threatening; 3 = required prolonged in patient
    hospitalization; 4 = disabling; 5 = a congenital anomaly/birth defect in offspring of study subject;
    6 = does not meet any of the above serious criteria, but may jeopardize the subject, and may require medical or
    surgical intervention to prevent one of the outcomes listed above.
*  Study day derived from imputed onset date.
** Duration derived from imputed onset date and/or end date.",
  prov_footer = "Do we need referential footnotes? Still not as the main ref notes are in the labels, conveniently."
) %>% head(50)

testthat::test_that("Direct pagination works fine", {
  testthat::expect_equal(nrow(lst_res), 50) # head() worked

  clw <- formatters::propose_column_widths(lst_res) / 2 + 1

  pg_lst <- testthat::expect_silent(paginate_listing(lst_res, lpp = 50, colwidths = floor(clw)))
  testthat::expect_equal(length(pg_lst), 10L)

  pg_lst <- testthat::expect_silent(paginate_listing(lst_res, page_type = "a4", font_size = 8))
  testthat::expect_equal(length(pg_lst), 6L)
})