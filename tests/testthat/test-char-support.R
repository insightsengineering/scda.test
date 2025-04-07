adsl <- random.cdisc.data::cadsl
adeg <- random.cdisc.data::cadeg

################################################################################
# Define script level parameters:
################################################################################
adsl_f <- adsl %>%
  select(USUBJID, ACTARM)

# Note:  We keep only post-baseline for analysis.
adeg_f <- adeg %>%
  filter(AVISIT %in% levels(adeg$AVISIT)[1:3]) %>%
  filter(PARAM %in% c("Heart Rate", "QT Duration", "RR Duration")) %>%
  var_relabel(
    PARAM = "Assessment",
    ANRIND = "Abnormality"
  ) %>%
  select(USUBJID, AVISIT, PARAM, PARAMCD, ANRIND, ACTARM) %>%
  mutate(PARCAT = case_when(PARAMCD == "QT" ~ "QT",
                            TRUE ~ "Other")) %>%
  mutate(ANRIND2 = paste(ANRIND, PARAMCD))  %>%
  mutate(ANRIND3 = paste(ANRIND, PARAM))

# Ensure character variables are converted to factors and empty strings and NAs are explicit missing levels.
adsl_f <- df_explicit_na(adsl_f)
adeg_f <- df_explicit_na(adeg_f)

adeg_f_1 <- adeg_f %>%
  filter(PARAMCD != "QT") %>%
  mutate(CRITDIR = "Direction 1") %>%
  mutate(ANRIND23 = ANRIND2)

adeg_f_2 <- adeg_f %>%
  mutate(CRITDIR = "Direction 2") %>%
  mutate(ANRIND23 = ANRIND3)

adeg_f <-rbind(adeg_f_1, adeg_f_2)



### mapping dataframe

mapdf <- unique(adeg_f %>% select(PARCAT, PARAMCD, CRITDIR, ANRIND23)) %>%
  arrange(PARCAT, PARAMCD, CRITDIR, ANRIND23) %>%
  mutate(across(where(is.factor), as.character))

### 3 versions of dataset
### CRITDIR not factor, all visits have available data for all PARAMCD
adeg_f1 <- adeg_f

### CRITDIR not factor, not all visits available data for all PARAMCD
### exclude QT from screening - this will generate the problem in 6.11
adeg_f <- adeg_f %>%
  filter(!(AVISIT == "SCREENING" & PARAMCD == "QT"))

### CRITDIR factor, not all visits available data
adeg_f2 <- adeg_f
### also part of the issue
adeg_f2$CRITDIR <- factor(adeg_f2$CRITDIR)






################################################################################
# Define layout and build table:
################################################################################


lyt <- basic_table(show_colcounts = TRUE,
                   colcount_format = "N=xx"
) %>%
  split_cols_by("ACTARM"
  )  %>%
  split_rows_by("AVISIT",
                label_pos = "topleft",
                section_div = " ",
                split_fun = drop_split_levels,
                split_label = "Time Point",
                child_labels = "visible"
  ) %>%
  split_rows_by("PARAMCD",
                split_label = "Test",
                label_pos = "topleft",
                indent_mod = 1L,
                child_labels = "visible",
                split_fun = trim_levels_to_map(mapdf)
  ) %>%
  split_rows_by("CRITDIR",
                split_label = "Direction",
                label_pos = "topleft",
                indent_mod = 1L,
                child_labels = "visible",
                split_fun = trim_levels_to_map(mapdf)
  )  %>%
  analyze(c("ANRIND23"),
          a_summary,
          extra_args = list(.stats = "count_fraction"),
          show_labels = "hidden",
          indent_mod = 1L
  )

testthat::test_that("CRITDIR is factor, and one PARAMCD is not available on all AVISITS", {
  res <- testthat::expect_silent(build_table(lyt, adeg_f2 , alt_counts_df = adsl_f))
  testthat::expect_snapshot(res)
})


testthat::test_that("CRITDIR is not factor, and all PARAMCD available on all AVISITS", {
  res <- testthat::expect_silent(build_table(lyt, adeg_f1 , alt_counts_df = adsl_f))
  testthat::expect_snapshot(res)
})

testthat::test_that("CRITDIR is not factor, and one PARAMCD is not available on all AVISITS", {
  res <- testthat::expect_silent(build_table(lyt, adeg_f , alt_counts_df = adsl_f))
  testthat::expect_snapshot(res)
})

