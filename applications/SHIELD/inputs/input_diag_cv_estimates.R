# use log-likelihood or more independent sources?

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
    output       = "cv"
) # gives 0.00939089750793856, log L = -275.144801890752


cv_Total <- calculate.error.terms(
    data.type    = "total.syphilis.diagnoses",
    data.source.1 = "cdc.sti.surveillance",
    data.source.2 = "lhd",
    output       = "cv"
) # no overlap, think of other 
