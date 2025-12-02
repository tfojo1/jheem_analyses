source("applications/SHIELD/R/shield_calculating_error_terms_for_likelihoods.R")

#Estimate used for all syphilis stages (too few data points in other stages)
PS_CV <- calculate.error.terms(
    data.type    = "ps.syphilis.diagnoses",
    data.source.1 = "cdc.aggregated.county",
    data.source.2 = "lhd",
    output       = "cv"
) # gives 0.0764791209420945, log L = -2005.39939971725


