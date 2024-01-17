# Extra libraries (suggested) for tests
library(dplyr)
library(tern)
library(lubridate)

# Data loading for tests
adsl_raw <- random.cdisc.data::cadsl
adab_raw <- random.cdisc.data::cadab
adae_raw <- random.cdisc.data::cadae
adaette_raw <- random.cdisc.data::cadaette
adcm_raw <- random.cdisc.data::cadcm
addv_raw <- random.cdisc.data::caddv
adeg_raw <- random.cdisc.data::cadeg
adex_raw <- random.cdisc.data::cadex
adhy_raw <- random.cdisc.data::cadhy
adlb_raw <- random.cdisc.data::cadlb
admh_raw <- random.cdisc.data::cadmh
adpc_raw <- random.cdisc.data::cadpc
adpp_raw <- random.cdisc.data::cadpp
adpc_raw <- random.cdisc.data::cadpc
adqs_raw <- random.cdisc.data::cadqs
adrs_raw <- random.cdisc.data::cadrs
adsub_raw <- random.cdisc.data::cadsub
adtte_raw <- random.cdisc.data::cadtte
advs_raw <- random.cdisc.data::cadvs

# Data loading for pharmaverse
adpp_pharmaverse <- pharmaverseadam::adpp
adsl_pharmaverse <- pharmaverseadam::adsl
adpc_pharmaverse <- pharmaverseadam::adpc
adlb_pharmaverse <- pharmaverseadam::adlb %>%
  mutate(AVALU = LBORRESU)

# skip_if_too_deep
skip_if_too_deep <- function(depth) { # nolintr
  checkmate::assert_number(depth, lower = 0, upper = 5)

  testing_depth <- getOption("TESTING_DEPTH")
  if (is.null(testing_depth)) testing_depth <- Sys.getenv("TESTING_DEPTH")

  testing_depth <- tryCatch(
    as.numeric(testing_depth),
    error = function(error) 3,
    warning = function(warning) 3
  )

  if (length(testing_depth) != 1 || is.na(testing_depth)) testing_depth <- 3

  if (testing_depth < depth) {
    testthat::skip(paste("testing depth", testing_depth, "is below current testing specification", depth))
  }
}

# expect_snapshot_ggplot - set custom plot dimensions
expect_snapshot_ggplot <- function(title, fig, width = NA, height = NA) {
  skip_if_not_installed("svglite")

  name <- paste0(title, ".svg")
  path <- tempdir()
  suppressMessages(ggplot2::ggsave(name, fig, path = path, width = width, height = height))
  path <- file.path(path, name)

  testthat::announce_snapshot_file(name = name)
  testthat::expect_snapshot_file(path, name)
}
