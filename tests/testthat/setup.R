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
set.seed(99)
adsl_pharmaverse <- pharmaverseadam::adsl %>%
  mutate(
    DCSREAS = sample(c("ADVERSE EVENT", ""), nrow(.), replace = TRUE, prob = c(0.08, 0.92)),
    DCSREAS = with_label(DCSREAS, "Discontinuation Reason")
  ) %>%
  filter(ACTARM != "Screen Failure")

adae_pharmaverse <- pharmaverseadam::adae %>%
  mutate(
    AETOXGR = sample(c("1", "2", "3", "4", "5"), nrow(.), replace = TRUE, prob = c(0.70, 0.20, 0.05, 0.045, 0.005)),
    AECONTRT = sample(c("Y"), nrow(.), replace = TRUE, prob = c(0.95)),
    ANL01FL = "Y",
    SMQ01NAM = ifelse(AEDECOD %in% c("NAUSEA", "VOMITING"), "SMQ01", NA_character_),
    SMQ01SC = ifelse(SMQ01NAM == "SMQ01", "BROAD", NA_character_),
    SMQ02NAM = ifelse(AEDECOD == "SKIN IRRITATION", "SMQ02", NA_character_),
    SMQ02SC = ifelse(SMQ02NAM == "SMQ02", "NARROW", NA_character_),
    CQ01NAM = ifelse(AEDECOD == "HEADACHE", "CQ01", NA_character_)
  )
set.seed(NULL)
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
