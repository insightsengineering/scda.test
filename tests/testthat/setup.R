# Extra libraries (suggested) for tests
library(dplyr)
library(scda.2022)
library(tern)
library(lubridate)

# Data loading for tests
adsl_raw <- scda::synthetic_cdisc_dataset("rcd_2023_03_17", "adsl")
adab_raw <- scda::synthetic_cdisc_dataset("rcd_2023_03_17", "adab")
adae_raw <- scda::synthetic_cdisc_dataset("rcd_2023_03_17", "adae")
adaette_raw <- scda::synthetic_cdisc_dataset("rcd_2023_03_17", "adaette")
adcm_raw <- scda::synthetic_cdisc_dataset("rcd_2023_03_17", "adcm")
addv_raw <- scda::synthetic_cdisc_dataset("rcd_2023_03_17", "addv")
adeg_raw <- scda::synthetic_cdisc_dataset("rcd_2023_03_17", "adeg")
adex_raw <- scda::synthetic_cdisc_dataset("rcd_2023_03_17", "adex")
adlb_raw <- scda::synthetic_cdisc_dataset("rcd_2023_03_17", "adlb")
admh_raw <- scda::synthetic_cdisc_dataset("rcd_2023_03_17", "admh")
adpc_raw <- scda::synthetic_cdisc_dataset("rcd_2023_03_17", "adpc")
adpp_raw <- scda::synthetic_cdisc_dataset("rcd_2023_03_17", "adpp")
adpc_raw <- scda::synthetic_cdisc_dataset("rcd_2023_03_17", "adpc")
adqs_raw <- scda::synthetic_cdisc_dataset("rcd_2023_03_17", "adqs")
adrs_raw <- scda::synthetic_cdisc_dataset("rcd_2023_03_17", "adrs")
adsub_raw <- scda::synthetic_cdisc_dataset("rcd_2023_03_17", "adsub")
adtte_raw <- scda::synthetic_cdisc_dataset("rcd_2023_03_17", "adtte")
advs_raw <- scda::synthetic_cdisc_dataset("rcd_2023_03_17", "advs")

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
