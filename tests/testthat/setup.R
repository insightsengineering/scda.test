# Extra libraries (suggested) for tests
library(dplyr)
library(rlistings)
library(scda.2022)
library(tern)

# Data loading for tests
adsl_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adsl")
adab_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adab")
adae_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adae")
adaette_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adaette")
adcm_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adcm")
addv_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "addv")
adeg_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adeg")
adex_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adex")
adlb_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adlb")
admh_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "admh")
adpc_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adpc")
adpp_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adpp")
adpc_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adpc")
adqs_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adqs")
adrs_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adrs")
adsub_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adsub")
adtte_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adtte")
advs_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "advs")

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
