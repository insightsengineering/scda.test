
if (!exists("level_reducer")) {
  source("setup.R", local = TRUE)
}

## pharmaverseadamjnj -------------------------------

# Data loading for pharmaverseadamjnj
adsl_jnj <- pharmaverseadamjnj::adsl

adae_jnj <- pharmaverseadamjnj::adae
adae_jnj <- level_reducer(adae_jnj, "AEDECOD",
  num_max_values = 33, num_of_rare_values = 1,
)

adcm_jnj <- pharmaverseadamjnj::adcm

adeg_jnj <- pharmaverseadamjnj::adeg

adex_jnj <- pharmaverseadamjnj::adex

adexsum_jnj <- pharmaverseadamjnj::adexsum

adlb_jnj <- pharmaverseadamjnj::adlb

advs_jnj <- pharmaverseadamjnj::advs
adttesaf_jnj <- pharmaverseadamjnj::adttesaf
adaeocmq_jnj <- pharmaverseadamjnj::adaeocmq

adpc_jnj <- pharmaverseadamjnj::adpc

## pharmaversesdtmjnj -------------------------------
ds_jnj <- pharmaversesdtmjnj::ds
mh_jnj <- pharmaversesdtmjnj::mh
