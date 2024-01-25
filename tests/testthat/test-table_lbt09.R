adhy_liver <-  full_join(
    (pharmaverseadam::adlb %>%
       filter(PARAMCD %in% c("BILI") & (AVISIT == "Baseline" | DTYPE == "MAXIMUM")) %>%
       select(USUBJID, ARM, AVISIT, PARAMCD, AVAL, ANRHI) %>%
       rename(BILI = PARAMCD,
              BILIVAL = AVAL,
              BILIRHI = ANRHI)),
    (pharmaverseadam::adlb %>%
       filter(PARAMCD %in% c("ALT", "AST") & (AVISIT == "Baseline" | DTYPE == "MAXIMUM")) %>%
       select(USUBJID, ARM, AVISIT, PARAMCD, AVAL, ANRHI) %>%
       rename(ALTAST = PARAMCD,
              ALTASTVAL = AVAL,
              ALTASTRHI = ANRHI)),
    by = join_by(USUBJID, ARM, AVISIT)
  ) %>%
  mutate(
      TBILI_CAT = factor(
        case_when(
          BILIVAL <= 2*BILIRHI ~ "Total Bilirubin <= 2xULN",
          BILIVAL > 2*BILIRHI ~ "Total Bilirubin > 2xULN"
        ),
        levels = c(
          "Total Bilirubin <= 2xULN",
          "Total Bilirubin > 2xULN"
        )
      ),
      ALTAST_CAT = factor(
        case_when(
          ALTAST %in% c("ALT") & (ALTASTVAL > 3*ALTASTRHI) ~ "ALT >3 - <= 5xULN",
          ALTAST %in% c("ALT") & (ALTASTVAL > 5*ALTASTRHI) ~ "ALT >5 - <= 10xULN",
          ALTAST %in% c("ALT") & (ALTASTVAL > 10*ALTASTRHI) ~ "ALT >10 - <= 20xULN",
          ALTAST %in% c("ALT") & (ALTASTVAL > 20*ALTASTRHI) ~ "ALT >20xULN",
          ALTAST %in% c("AST") & (ALTASTVAL > 3*ALTASTRHI) ~ "AST >3 - <= 5xULN",
          ALTAST %in% c("AST") & (ALTASTVAL > 5*ALTASTRHI) ~ "ALT >5 - <= 10xULN",
          ALTAST %in% c("AST") & (ALTASTVAL > 10*ALTASTRHI) ~ "AST >10 - <= 20xULN",
          ALTAST %in% c("AST") & (ALTASTVAL > 20*ALTASTRHI) ~ "AST >20xULN",
          TRUE ~ "Criteria not met"
        ),
        levels = c(
          "ALT >3 - <= 5xULN", "ALT >5 - <= 10xULN", "ALT >10 - <= 20xULN", "ALT > 20xULN",
          "AST >3 - <= 5xULN", "AST >5 - <= 10xULN", "AST >10 - <= 20xULN", "AST > 20xULN",
          "Criteria not met"
        )
      ),
      ALTAST_ind = factor(
        case_when(
          ALTAST == "ALT" ~ "ALT",
          ALTAST == "AST" ~ "AST"
        ),
        levels = c("ALT", "AST")
      )
    )

map <- data.frame(
    ALTAST_ind = c(rep("ALT", 5), rep("AST", 5)),
    ALTAST_CAT = c(
      "ALT >3 - <= 5xULN",
      "ALT >5 - <= 10xULN",
      "ALT >10 - <= 20xULN",
      "20" = "ALT > 20xULN",
      "Criteria not met",
      "AST >3 - <= 5xULN",
      "AST >5 - <= 10xULN",
      "AST >10 - <= 20xULN",
      "AST > 20xULN",
      "Criteria not met"
    ),
    stringsAsFactors = FALSE
  )

testthat::test_that("LBT09 variant 1 works as expected", {
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_cols_by("AVISIT") %>%
    split_rows_by("TBILI_CAT") %>%
    # below split helps us get the right denominator between ALT/AST but it can be hidden
    split_rows_by("ALTAST_ind", split_fun = trim_levels_to_map(map), child_labels = "hidden") %>%
    count_occurrences(
      vars = "ALTAST_CAT",
      .stats = "fraction",
      denom = "n",
      drop = FALSE
    ) %>%
    append_topleft("Liver Laboratory Test Criterion") %>%
    build_table(df = adhy_liver)

  # trim away rows with criteria not met
  criteria_fun <- function(tr) {
    row_label <- obj_label(tr)
    ifelse(row_label == "Criteria not met", TRUE, FALSE)
  }

  result <- result %>% trim_rows(criteria = criteria_fun)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("LBT09 variant 2 works as expected", {
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_cols_by("AVISIT") %>%
    split_rows_by(
      "TBILI_CAT",
      split_fun = remove_split_levels("Total Bilirubin > 2xULN and Alkaline Phosphatase <= 2xULN")
    ) %>%
    # below split helps us get the right denominator between ALT/AST but it can be hidden
    split_rows_by("ALTAST_ind", split_fun = trim_levels_to_map(map), child_labels = "hidden") %>%
    count_occurrences(
      vars = "ALTAST_CAT",
      .stats = "fraction",
      denom = "n",
      drop = FALSE
    ) %>%
    append_topleft("Liver Laboratory Test Criterion") %>%
    build_table(df = adhy_liver)

  # trim away rows with criteria not met
  criteria_fun <- function(tr) {
    row_label <- obj_label(tr)
    ifelse(row_label == "Criteria not met", TRUE, FALSE)
  }

  result <- result %>% trim_rows(criteria = criteria_fun)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
