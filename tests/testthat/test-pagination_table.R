# All of these tests are based on AET04 variant 1 and mainly concern pagination,
# i.e. paginate_table and its parameters.

# Data pre-processing
adsl <- adsl_raw
adae <- adae_raw %>%
  dplyr::mutate(
    AEDECOD = as.character(AEDECOD),
    AEBODSYS = as.character(AEBODSYS),
  )
gr_grp <- list(
  "- Any Grade -" = c("1", "2", "3", "4", "5"),
  "Grade 1-2" = c("1", "2"),
  "Grade 3-4" = c("3", "4"),
  "Grade 5" = "5"
)

# Raw result for future pruning
raw_result <- basic_table() %>%
  split_cols_by("ACTARM") %>%
  add_colcounts() %>%
  count_occurrences_by_grade(
    var = "AETOXGR",
    grade_groups = gr_grp
  ) %>%
  split_rows_by("AEBODSYS",
    split_fun = trim_levels_in_group("AETOXGR"),
    child_labels = "visible", nested = TRUE
  ) %>%
  summarize_occurrences_by_grade(
    var = "AETOXGR",
    grade_groups = gr_grp
  ) %>%
  split_rows_by("AEDECOD",
    split_fun = trim_levels_in_group("AETOXGR"),
    child_labels = "visible", nested = TRUE
  ) %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = "unique",
    .labels = "- Any Grade -"
  ) %>%
  count_occurrences_by_grade(
    var = "AETOXGR",
    grade_groups = gr_grp[-1],
    .indent_mods = -1L
  ) %>%
  build_table(adae, alt_counts_df = adsl) %>%
  sort_at_path(
    path = "AEBODSYS",
    scorefun = cont_n_allcols,
    decreasing = TRUE
  ) %>%
  sort_at_path(
    path = c("AEBODSYS", "*", "AEDECOD"),
    scorefun = cont_n_allcols,
    decreasing = TRUE
  )

testthat::test_that("Direct pagination with standard values", {
  # Standard, direct call
  pag_res <- testthat::expect_silent(paginate_table(raw_result, font_size = 12))
  testthat::expect_equal(sapply(pag_res, nrow), c(55, 22))
  testthat::expect_equal(sapply(pag_res, ncol), c(3, 3))
})

testthat::test_that("Pagination with specific column widths and minimum lines per page", {
  # With combo of rows and cols per page
  std_col_widths <- propose_column_widths(raw_result)
  # Min h-width: first value is the row label, second is the largest content, 3 is the inter-space
  cpp_width <- std_col_widths[1] + max(std_col_widths[-1]) + 3
  pag_res <- testthat::expect_silent(
    paginate_table(raw_result, cpp = cpp_width, lpp = 13) # 13 is the minimum -> automatic way?
  )
  testthat::expect_equal(length(pag_res), 11 * 3) # 11 because 3 branches (out of 8) have 2 leaves
  testthat::expect_snapshot(pag_res[9:10]) # randomly picked to have a comparison
})

# More decoration
res <- raw_result
main_title(res) <- "main title with some new \nline"
subtitles(res) <- c("sub", "-------", "titles")
main_footer(res) <- "main footer"
prov_footer(res) <- "prov \nfooter that has a lot of \nnew \nlines"
top_left(res) <- "SOME TOPLEFT"
table_inset(res) <- 5
fnotes_at_path(
  res,
  c(make_row_df(res)$path[[7]]),
  c("B: Placebo") # nolint
) <- "Some notes"

testthat::test_that("Pagination works also if table is decorated", {
  clw <- c(10, 8, 8, 10) # random values to have "some" wrapping

  # Std calculations for minimum lpp
  # title (2) + subtitle (3) + empty line (1) + div (1) + header (3) +
  # + nrows (x) + ref footnote (3) + footer (1+2 empty lines) + prov footer (4)
  testthat::expect_silent(paginate_table(raw_result, lpp = 13))
  testthat::expect_error(suppressMessages(paginate_table(raw_result, lpp = 12)))
  # base is: 20 + x [x from paginate_table(raw_result) is min 13 - 3 (header)]
  # -> 30
  # for pagination with decoration and no wrapping we expect 27
  lpp_min <- 28 # -2 less because of ref fnotes
  cpp_min <- 72 # 70 is the std now, this table is 72
  testthat::expect_silent(paginate_table(res, lpp = lpp_min, cpp = cpp_min))
  testthat::expect_error(suppressMessages(paginate_table(res, lpp = lpp_min - 1, cpp = cpp_min)))

  # for clw above this is 42 (15 more lines due to wrapping)
  lpp_min <- 42
  pg_tbl_no_clw <- paginate_table(res, lpp = lpp_min, cpp = cpp_min)
  pg_tbl_w_clw <- paginate_table(res, lpp = lpp_min, colwidths = clw, cpp = cpp_min)
  testthat::expect_error(paginate_table(res, lpp = lpp_tmp - 1, colwidths = clw, cpp = cpp_min))

  res1 <- toString(pg_tbl_no_clw[[1]], widths = clw)
  res2 <- toString(res[
    1:21, , # found manually
    keep_titles = TRUE,
    keep_footers = TRUE,
    keep_topleft = TRUE
  ], widths = clw)

  testthat::expect_identical(res1, res2)
  testthat::expect_equal(length(pg_tbl_no_clw), length(pg_tbl_w_clw) - 7)

  testthat::expect_snapshot(cat(res1))

  testthat::expect_snapshot(cat(toString(pg_tbl_no_clw[[3]], widths = clw)))
  testthat::expect_snapshot(cat(toString(pg_tbl_w_clw[[3]], widths = clw)))
})

testthat::test_that("Pagination works for page types", {
  # Other parameters: page type
  pag_res <- paginate_table(res, page_type = "letter")
  testthat::expect_snapshot(sapply(pag_res, nrow))
  testthat::expect_snapshot(pag_res[[2]])
  pag_res <- paginate_table(res, page_type = "a4")
  testthat::expect_snapshot(sapply(pag_res, nrow))
  testthat::expect_snapshot(pag_res[[2]])
  pag_res <- paginate_table(res, page_type = "legal")
  testthat::expect_snapshot(sapply(pag_res, nrow))
  testthat::expect_snapshot(pag_res[[1]])
})

testthat::test_that("Pagination works for page width and height", {
  # Other parameters: page height and width
  pag_res <- paginate_table(res, pg_width = 7, pg_height = 10)
  testthat::expect_snapshot(sapply(pag_res, nrow))
  testthat::expect_snapshot(pag_res[2])
})

testthat::test_that("Pagination works for page types", {
  # Other parameters: all together
  pag_res <- paginate_table(
    res,
    landscape = TRUE,
    lineheight = 2,
    font_size = 7,
    font_family = "Courier",
    margins = c(top = 1, bottom = 1, left = 3, right = 3)
  )
  testthat::expect_snapshot(sapply(pag_res, nrow))
  testthat::expect_snapshot(pag_res[5:6]) # randomly picked to have a comparison
})

testthat::test_that("AET04 variant 2 page_by pagination tests", {
  adae <- adae %>%
    dplyr::filter(ACTARM == "A: Drug X") %>%
    dplyr::mutate(AEBODSYS = factor(AEBODSYS))

  lyt <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    split_rows_by("AEBODSYS",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE,
      page_by = TRUE
    ) %>%
    summarize_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEDECOD",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "- Any Grade -"
    ) %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp[-1],
      .indent_mods = -1L
    )

  result <- lyt %>%
    build_table(adae, alt_counts_df = adsl) %>%
    sort_at_path(
      path = "AEBODSYS",
      scorefun = cont_n_allcols,
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = cont_n_allcols,
      decreasing = TRUE
    )

  res <- testthat::expect_silent(result)
  pag_res <- testthat::expect_silent(paginate_table(res))
  testthat::expect_setequal(
    sapply(pag_res, page_titles),
    paste0("AEBODSYS: ", levels(adae$AEBODSYS))
  )
  testthat::expect_snapshot(pag_res[c(2, 5)]) # randomly picked to have a comparison
})
