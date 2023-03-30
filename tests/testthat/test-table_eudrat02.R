adae <- adae_raw

# Ensure character variables are converted to factors and empty strings and NAs are explicit missing levels.
adae <- df_explicit_na(adae)

adae_serious <- adae %>% filter(AESER == "Y", SAFFL == "Y")
adae_serious_arm <- adae_serious %>% filter(ARM == "A: Drug X")

filters_list <- list(
  related = with_label(c(AEREL = "Y"), "Events (Related)"),
  fatal = with_label(c(AESDTH = "Y"), "Events (Fatal)"),
  fatal_related = with_label(c(AEREL = "Y", AESDTH = "Y"), "Events (Fatal & Related)")
)

testthat::test_that("EUDRAT02 is produced correctly", {
  split_fun <- drop_split_levels

  lyt <- basic_table() %>%
    summarize_patients_events_in_cols(
      filters_list = filters_list,
      empty_stats = c("all", "related", "fatal", "fatal_related"),
      custom_label = "Total number of patients with at least one serious adverse event"
    ) %>%
    split_rows_by("AEBODSYS",
                  nested = FALSE,
                  split_fun = split_fun,
                  indent_mod = -1L,
                  label_pos = "topleft",
                  split_label = obj_label(adae_serious_arm$AEBODSYS)
    ) %>%
    split_rows_by("AEDECOD",
                  split_fun = split_fun,
                  label_pos = "topleft",
                  split_label = obj_label(adae_serious_arm$AEDECOD)
    ) %>%
    summarize_patients_events_in_cols(
      filters_list = filters_list,
      col_split = FALSE
    )

  result <- build_table(lyt, adae_serious_arm)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
