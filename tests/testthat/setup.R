# Extra libraries (suggested) for tests
library(dplyr)
library(scda.2022)
library(tern)
library(lubridate)

# Data loading for tests
adsl_raw <- scda::synthetic_cdisc_dataset("latest", "adsl")
adab_raw <- scda::synthetic_cdisc_dataset("latest", "adab")
adae_raw <- scda::synthetic_cdisc_dataset("latest", "adae")
adaette_raw <- scda::synthetic_cdisc_dataset("latest", "adaette")
adcm_raw <- scda::synthetic_cdisc_dataset("latest", "adcm")
addv_raw <- scda::synthetic_cdisc_dataset("latest", "addv")
adeg_raw <- scda::synthetic_cdisc_dataset("latest", "adeg")
adex_raw <- scda::synthetic_cdisc_dataset("latest", "adex")
adhy_raw <- scda::synthetic_cdisc_dataset("latest", "adhy")
adlb_raw <- scda::synthetic_cdisc_dataset("latest", "adlb")
admh_raw <- scda::synthetic_cdisc_dataset("latest", "admh")
adpc_raw <- scda::synthetic_cdisc_dataset("latest", "adpc")
adpp_raw <- scda::synthetic_cdisc_dataset("latest", "adpp")
adpc_raw <- scda::synthetic_cdisc_dataset("latest", "adpc")
adqs_raw <- scda::synthetic_cdisc_dataset("latest", "adqs")
adrs_raw <- scda::synthetic_cdisc_dataset("latest", "adrs")
adsub_raw <- scda::synthetic_cdisc_dataset("latest", "adsub")
adtte_raw <- scda::synthetic_cdisc_dataset("latest", "adtte")
advs_raw <- scda::synthetic_cdisc_dataset("latest", "advs")

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
