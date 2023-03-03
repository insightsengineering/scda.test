testthat::test_that("DSUR4 listing is produced correctly", {
  out <- adsl_raw %>%
    filter(DTHFL == "Y") %>%
    mutate(ID = paste(SITEID, SUBJID, sep = "/")) %>%
    select(ARM, ID, DTHCAUS)
  death_num <- length(unique(out$ID))

  formatters::var_labels(out) <- c(
    ARM = "Treatment Group",
    ID = "Center/Patient ID",
    DTHCAUS = "Cause of Death"
  )

  testthat::expect_message(result <- as_listing(
    out,
    key_cols = c("ARM"),
    disp_cols = names(out),
    main_title = "Listing of Patients Who Died During Reporting Period",
    subtitles = paste("Number of patient deaths during reporting period =", death_num)
  ), "sorting incoming data by key columns")

  testthat::expect_snapshot(result)
})
