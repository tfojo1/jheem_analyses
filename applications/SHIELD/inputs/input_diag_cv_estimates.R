source("/applications/SHIELD/R/shield_calculating_error_terms_for_likelihoods.R")
cv_PS_1 <- calculate.error.terms(
    data.type    = "ps.syphilis.diagnoses",
    data.source.1 = "cdc.sti",
    data.source.2 = "cdc.sti.surveillance.reports",
    output       = "cv"
) # gives 0.000104827043686166, log L = -88.5983303613971


cv_PS_2 <- calculate.error.terms(
    data.type    = "ps.syphilis.diagnoses",
    data.source.1 = "cdc.aggregated.county",
    data.source.2 = "lhd",
    output       = "cv"
) # gives 0.0764791209420945, log L = -2005.39939971725


sd_PS_1 <- calculate.error.terms(
    data.type    = "ps.syphilis.diagnoses",
    data.source.1 = "cdc.aggregated.county",
    data.source.2 = "lhd",
    output       = "cv.and.sd"
) # gives 460.36

sd_PS_2 <- calculate.error.terms(
    data.type    = "ps.syphilis.diagnoses",
    data.source.1 = "cdc.aggregated.county",
    data.source.2 = "lhd",
    output       = "exponent.of.variance"
) 

sd_PS_2 <- calculate.error.terms(
    data.type    = "ps.syphilis.diagnoses",
    data.source.1 = "cdc.aggregated.county",
    data.source.2 = "lhd",
    output       = "cv.and.exponent.of.variance.eq.1"
) 

sd_PS_2 <- calculate.error.terms(
    data.type    = "ps.syphilis.diagnoses",
    data.source.1 = "cdc.aggregated.county",
    data.source.2 = "lhd",
    output       = "cv.and.exponent.of.variance"
) 


cv_EL_1 <- calculate.error.terms(
    data.type    = "early.syphilis.diagnoses",
    data.source.1 = "cdc.sti",
    data.source.2 = "cdc.sti.surveillance.reports",
    output       = "cv"
) # gives 0

cv_EL_2 <- calculate.error.terms(
    data.type    = "early.syphilis.diagnoses",
    data.source.1 = "cdc.aggregated.county",
    data.source.2 = "lhd",
    output       = "cv"
) # gives 0.430017502673929, log L = -2523.19107850843


sd_EL_2 <- calculate.error.terms(
    data.type    = "early.syphilis.diagnoses",
    data.source.1 = "cdc.aggregated.county",
    data.source.2 = "lhd",
    output       = "sd"
) # gives 709.639386189259

cv_LL_1 <- calculate.error.terms(
    data.type    = "unknown.duration.or.late.syphilis.diagnoses",
    data.source.1 = "cdc.sti",
    data.source.2 = "cdc.sti.surveillance.reports",
    output       = "cv"
) # gives 0


cv_LL_2 <- calculate.error.terms(
    data.type    = "unknown.duration.or.late.syphilis.diagnoses",
    data.source.1 = "cdc.aggregated.county",
    data.source.2 = "lhd",
    output       = "cv and sd"
) # gives 0.00939089750793856, log L = -275.144801890752

sd_LL_2 <- calculate.error.terms(
    data.type    = "unknown.duration.or.late.syphilis.diagnoses",
    data.source.1 = "cdc.aggregated.county",
    data.source.2 = "lhd",
    output       = "sd"
) # gives 1424.82432432432


cv_Total <- calculate.error.terms(
    data.type    = "total.syphilis.diagnoses",
    data.source.1 = "cdc.sti.surveillance",
    data.source.2 = "lhd",
    output       = "cv"
) # no overlap, think of other 
