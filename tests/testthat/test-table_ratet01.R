anl <- adtte_raw %>% filter(PARAMCD == "TNE")

# Ensure number of exacerbation is a factor and NAs are explicit missing levels.
anl$AVAL_f <- as.factor(anl$AVAL)
anl <- df_explicit_na(anl)

testthat::test_that("RATET01 is produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM",
      ref_group = "B: Placebo",
      split_fun = ref_group_position("first")
    ) %>%
    analyze_vars(
      "AVAL_f",
      var_labels = "Number of exacerbations per patient",
      .stats = c("count_fraction"),
      .formats = c("count_fraction" = "xx (xx.xx%)"),
      .labels = c("Number of exacerbations per patient")
    ) %>%
    summarize_glm_count(
      vars = "AVAL",
      variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = NULL),
      conf_level = 0.95,
      distribution = "poisson",
      rate_mean_method = "emmeans",
      var_labels = "Unadjusted exacerbation rate (per year) with offset",
      table_names = "unadj-offest",
      .stats = c("rate"),
      .labels = c(rate = "Rate")
    ) %>%
    summarize_glm_count(
      vars = "AVAL",
      variables = list(arm = "ARM", covariates = NULL),
      conf_level = 0.95,
      distribution = "poisson",
      rate_mean_method = "emmeans",
      var_labels = "Unadjusted exacerbation rate (per year)",
      table_names = "unadj",
      .stats = c("rate"),
      .labels = c(rate = "Rate")
    ) %>%
    summarize_glm_count(
      vars = "AVAL",
      variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = c("REGION1")),
      conf_level = 0.95,
      distribution = "quasipoisson",
      rate_mean_method = "ppmeans",
      var_labels = "Adjusted (QP) exacerbation rate (per year) with offset",
      table_names = "adj-qp-offest",
      .stats = c("rate", "rate_ci", "rate_ratio", "rate_ratio_ci", "pval"),
      .labels = c(
        rate = "Rate", rate_ci = "Rate CI", rate_ratio = "Rate Ratio",
        rate_ratio_ci = "Rate Ratio CI", pval = "p-value"
      )
    ) %>%
    summarize_glm_count(
      vars = "AVAL",
      variables = list(arm = "ARM", covariates = c("REGION1")),
      conf_level = 0.95,
      distribution = "quasipoisson",
      rate_mean_method = "emmeans",
      var_labels = "Adjusted (QP) exacerbation rate (per year)",
      table_names = "adj-qp",
      .stats = c("rate", "rate_ci", "rate_ratio", "rate_ratio_ci", "pval"),
      .labels = c(
        rate = "Rate", rate_ci = "Rate CI", rate_ratio = "Rate Ratio",
        rate_ratio_ci = "Rate Ratio CI", pval = "p-value"
      )
    ) %>%
    summarize_glm_count(
      vars = "AVAL",
      variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = c("REGION1")),
      conf_level = 0.95,
      distribution = "negbin",
      rate_mean_method = "emmeans",
      var_labels = "Adjusted (NB) exacerbation rate (per year) with offset",
      table_names = "adj-nb-offest",
      .stats = c("rate", "rate_ci", "rate_ratio", "rate_ratio_ci", "pval"),
      .labels = c(
        rate = "Rate", rate_ci = "Rate CI", rate_ratio = "Rate Ratio",
        rate_ratio_ci = "Rate Ratio CI", pval = "p-value"
      )
    ) %>%
    summarize_glm_count(
      vars = "AVAL",
      variables = list(arm = "ARM", covariates = c("REGION1")),
      conf_level = 0.95,
      distribution = "negbin",
      rate_mean_method = "emmeans",
      var_labels = "Adjusted (NB) exacerbation rate (per year)",
      table_names = "adj-nb",
      .stats = c("rate", "rate_ci", "rate_ratio", "rate_ratio_ci", "pval"),
      .labels = c(
        rate = "Rate", rate_ci = "Rate CI", rate_ratio = "Rate Ratio",
        rate_ratio_ci = "Rate Ratio CI", pval = "p-value"
      )
    )

  result <- build_table(
    lyt = lyt,
    df = anl
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
