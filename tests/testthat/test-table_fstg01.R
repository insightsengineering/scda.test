adsl_f <- adsl_raw %>%
  select(STUDYID, USUBJID, ARMCD, ARM, SEX, BMRKR2, STRATA1, STRATA2)

adrs_f <- adrs_raw %>%
  filter(PARAMCD == "INVET") %>%
  select(STUDYID, USUBJID, PARAMCD, AVISIT, AVALC)

anl <- inner_join(adsl_f, adrs_f, by = c("STUDYID", "USUBJID"))
anl <- df_explicit_na(anl)
anl_labels <- var_labels(anl)

anl_rsp_arms_ab <- anl %>%
  mutate(is_rsp = AVALC %in% c("CR", "PR")) %>%
  filter(ARMCD %in% c("ARM B", "ARM A")) %>%
  mutate(
    ARMCD = relevel(ARMCD, ref = "ARM B") %>%
      droplevels(),
    ARM = relevel(ARM, ref = "B: Placebo") %>%
      droplevels()
  ) %>%
  droplevels()
var_labels(anl_rsp_arms_ab) <- c(anl_labels, is_rsp = "Is Responder")

testthat::test_that("FSTG01 variant 1 is produced correctly", {
  df <- extract_rsp_subgroups(
    variables = list(
      rsp = "is_rsp",
      arm = "ARM",
      subgroups = c("SEX", "BMRKR2"),
      strata_var = "STRATA2"
    ),
    data = anl_rsp_arms_ab,
    conf_level = 0.95
  )

  result <- basic_table() %>%
    tabulate_rsp_subgroups(df, vars = c("n_tot", "n", "n_rsp", "prop", "or", "ci"))

  v1 <- g_forest(tbl = result)

  expect_snapshot_ggplot("v1", v1, width = 15, height = 5)
})

testthat::test_that("FSTG01 variant 2 is produced correctly", {
  anl_rsp_comb_arms_ac <- anl %>%
    mutate(is_rsp = AVALC %in% c("CR", "PR")) %>%
    filter(ARMCD %in% c("ARM B", "ARM A", "ARM C")) %>%
    mutate(
      ARMCD = relevel(ARMCD, ref = "ARM B") %>%
        droplevels() %>%
        combine_levels(levels = c("ARM A", "ARM C")),
      ARM = relevel(ARM, ref = "B: Placebo") %>%
        droplevels() %>%
        combine_levels(levels = c("A: Drug X", "C: Combination")),
      # reorder levels of SEX
      SEX = forcats::fct_relevel(SEX, "M", "F"),
      # reorder levels of STRATA1 by frequency
      STRATA1 = forcats::fct_infreq(STRATA1)
    ) %>%
    droplevels()
  var_labels(anl_rsp_comb_arms_ac) <- c(anl_labels, is_rsp = "Is Responder")

  df <- extract_rsp_subgroups(
    variables = list(
      rsp = "is_rsp",
      arm = "ARMCD",
      subgroups = c("SEX", "BMRKR2"),
      strata_var = "STRATA2"
    ),
    data = anl_rsp_comb_arms_ac,
    conf_level = 0.95
  )

  result <- basic_table() %>%
    tabulate_rsp_subgroups(df, vars = c("n_tot", "n", "n_rsp", "prop", "or", "ci"))

  v2 <- g_forest(tbl = result)

  expect_snapshot_ggplot("v2", v2, width = 15, height = 5)
})

testthat::test_that("FSTG01 variant 3 is produced correctly", {
  df <- extract_rsp_subgroups(
    variables = list(
      rsp = "is_rsp",
      arm = "ARM",
      subgroups = c("SEX", "BMRKR2"),
      strata_var = "STRATA2"
    ),
    data = anl_rsp_arms_ab,
    conf_level = 0.90
  )

  result <- basic_table() %>%
    tabulate_rsp_subgroups(df, vars = c("n_tot", "or", "ci"))

  v3 <- g_forest(tbl = result)

  expect_snapshot_ggplot("v3", v3, width = 10, height = 3)
})

testthat::test_that("FSTG01 variant 4 is produced correctly", {
  df <- extract_rsp_subgroups(
    variables = list(
      rsp = "is_rsp",
      arm = "ARM",
      subgroups = c("SEX", "BMRKR2"),
      strata_var = "STRATA2"
    ),
    data = anl_rsp_arms_ab,
    conf_level = 0.95
  )

  result <- basic_table() %>%
    tabulate_rsp_subgroups(df, vars = c("n_tot", "n", "n_rsp", "prop", "or", "ci"))

  v4 <- g_forest(
    tbl = result,
    col_symbol_size = NULL
  )

  expect_snapshot_ggplot("v4", v4, width = 20, height = 5)
})

testthat::test_that("FSTG01 variant 5 is produced correctly", {
  anl_cr_arms_ab <- anl %>%
    mutate(is_rsp = AVALC == "CR") %>%
    filter(ARMCD %in% c("ARM B", "ARM A")) %>%
    mutate(
      ARMCD = relevel(ARMCD, ref = "ARM B") %>%
        droplevels(),
      ARM = relevel(ARM, ref = "B: Placebo") %>%
        droplevels()
    ) %>%
    droplevels()
  var_labels(anl_cr_arms_ab) <- c(anl_labels, is_rsp = "Is CR")

  df <- extract_rsp_subgroups(
    variables = list(
      rsp = "is_rsp",
      arm = "ARM",
      subgroups = c("SEX", "BMRKR2"),
      strata_var = "STRATA2"
    ),
    data = anl_cr_arms_ab,
    conf_level = 0.95
  )

  result <- basic_table() %>%
    tabulate_rsp_subgroups(df, vars = c("n_tot", "n", "n_rsp", "prop", "or", "ci"))

  v5 <- g_forest(tbl = result)

  expect_snapshot_ggplot("v5", v5, width = 20, height = 5)
})
