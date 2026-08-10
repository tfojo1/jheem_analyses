


# uses estiamtes from:
# source("../jheem_analyses/applications/SHIELD/inputs/x-input_untreated_syphilis_progression_rates.R)
# source("../jheem_analyses/applications/SHIELD/inputs/input_congenital_relative_risks.R")
# source(../jheem_analyses/applications/SHIELD/inputs/input_fraction_hiv_test_by_age.R)
# source(../jheem_analyses/applications/SHIELD/inputs/input_syphilis_misclassification_error.R)

# what are the citation numbers?
add.parameter <- function(params, param.name,
                          value,
                          ci.lower,
                          ci.upper,
                          citation=NA,
                          comment=NA){
    params$values[param.name] = value
    params$ci.lower[param.name] = ci.lower
    params$ci.upper[param.name] = ci.upper
    params$citation[[param.name]] = list(citation)
    params$comment[param.name] = comment
    
    params
}
SHIELD_BASE_PARAMETER = list(values=numeric(),
                             ci.lower=numeric(),
                             ci.upper=numeric(),
                             citation=list(),
                             comment=character())

# ci's are not used
# citation numbers are pubmed ID, they're for our own records'

# *** SYPHILIS NATURAL HISTORY *** ##-----
## STATE DURATIONS ----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'duration.primary',
                                      4/52, 0,0) #2-6weeks 
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'duration.secondary',
                                      2/12, 0,0) #1-3 months
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'duration.early.latent',
                                      9/12, 0,0) #9-11 months (to sum to 1year with secondary)

# Assuming a duration of 1 month for tertiary and cns: they're symptomatic stages, followed by immediate testing 
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'duration.tertiary',
                                      1/12,0,0) #average of 1 month
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'duration.cns',
                                      1/12,0,0) #average of 1 month

## TRANSITION RATES ----
# RELAPSE: 25% of persons leaving EL go to secondary, the rest go to LL
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prop.early.latent.to.secondary',
                                      0.25,0,0)

# Early stages to CNS: #used for primary, secondary and EL
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.early.stage.to.cns',
                                      0.015 ,0,0) #0.012-0.017
# Late Latent to CNS: inputs/untreated_syphilis_progression_rates.R
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.late.latent.to.cns.male',
                                      0.0045 ,0,0) #[95% CI: 0.0028 - 0.0062]
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.late.latent.to.cns.female',
                                      0.0022 ,0,0) #[95% CI: 0.0014 - 0.0031] 

# Late Latent to Tertiary: inputs/untreated_syphilis_progression_rates.R
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.late.latent.to.tertiary.male',
                                      0.0102,0,0) #0.0085 - 0.0119
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.late.latent.to.tertiary.female',
                                      0.0099 ,0,0) #0.0079 – 0.0119
## *** CARE CASCADE *** ----
## SYMPTOMATIC INFECTIONS ----                        
## Fraction with symptomatic disesase: changed as calib params
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.symptomatic.primary.msm',
                                      0.25, 0,0)
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rr.prp.symptomatic.primary.female', #relative to MSM
                                      0.66, 0,0)
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rr.prp.symptomatic.primary.heterosexual_male',#relative to MSM
                                      1, 0,0)
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.symptomatic.secondary',
                                      0.16, 0,0)                                      
## HIV TESTING ##-----
# inputs/input_fraction_hiv_test_by_age.R
# what fraction of tests reported in 15-19 agegroup are carried among 18-19 year olds
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'fraction.hiv.tests.18.19.among.15.19',
                                      0.62, 0,0,
                                      citation = "input_fraction_hiv_test_by_age.R")
## MISCLASSIFICATION ERROR -----
# source("applications/SHIELD/inputs/input_syphilis_misclassification_error.R")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'fraction.el.misclassified.ll',
                                      0.096, 0,0)

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'fraction.ll.misclassified.el',
                                      0.272, 0,0)

#*** TREATMENTS INITIATION  **** ## ---- 
#*#'@PK:double check
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.treated.immediately.following.screening', 
                                      0.89,0,0)

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.treated.immediately.following.symptomatic.testing', 
                                      0.89,0,0)
#differences by stage of infection (early vs late) was too small to include 

#if someone is diagnosed and doesn't receive immediate treatment, what is the rate of treatment
#'@Todd: should we separate this based on symptoms? or for pregnant women? 
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.treatment.after.delay', 
                                      1.91,0,0) 

# *** TRANSMISSION ****  ----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER,'secondary.transmissibility',  
                                      1,0,0)  # Max value
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER,'primary.rel.secondary.transmissibility',  
                                      1,0,0) 

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER,'el.rel.secondary.transmissibility',  
                                      .25,0,0)

## SEXUAL TRANSMISSION RATES ---- ##----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'male.to.female.sexual.transmission',
                                      4.75, 2.4, 7.1,
                                      citation=26362321) 
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'female.to.male.sexual.transmission',
                                      3.75, 1.8, 5.6,
                                      citation=26362321) 
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'male.to.male.sexual.transmission',
                                      5, 2.5, 7.5,
                                      citation=26362321) 

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.msm.sex.with.female',
                                      0.18, 0.18*5, 0.18*2,
                                      citation=9525438) 


# *** NEW BIRTHS ---- ##----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'ratio.birth.male.to.female',
                                      1.048,0,0) # 1.04, 1.06
## Prop Multibirths
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.births.multi.born', #proportion of births that are multibirth
                                      0.031,0,0) #we are not including the trend here



# *** CONTACT TRACING ---- ## ----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'B.MODEL.CONTACT.TRACING',
                                      0,0,0)   # SWITCH OO TURN ON/OFF CONTACT TRACING

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prop.index.cases.reached.for.contact.tracing',
                                      0.8,0,0)   #0.3, 0.98
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'contacts.diagnosed.treated.per.index.case',
                                      0.1, 0,0) #.05, 0.2,
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'contacts.empirically.treated.infected.per.index.case',
                                      0.1, 0,0) #0.04, 0.19,
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.infected.contacts.in.primary',
                                      0.1425, 0,0)
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.infected.contacts.in.secondary',
                                      0.4275, 0,0)
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.infected.contacts.in.early.latent',
                                      0.215, 0,0)

# *** CONGENITAL SYPHILIS ---- ##----
# Boolean variable to control prenatal care and congenital syphilis as a switch
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'B.MODEL.PRENATAL.AND.CS',
                                     0,0,0) # SWITCH TO TURN ON/OFF PRENATAL CARE ADN CONGENITAL SYPHILIS

## ---- Prob of Vertical Transmission Based on Disease Stage -----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prob.vertical.transmission.mothers.early.syphilis',
                                      0.5,0,0) #0.3 - 0.70 
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prob.vertical.transmission.mothers.late.syphilis',
                                      0.1,0,0)# 0.02 - 0.2

## ---- Risk Ratios Based on Prenatal Cares timing  ----
# inputs/input_congenital_relative_risks.R
# cs_relative_risks 
# first    second     third      none 
# 0.3061055 0.5160727 0.8182090 1.000000
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rr.congenital.syphilis.no.prenatal.care',
                                      1.0, 0,0)
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rr.congenital.syphilis.prenatal.care.first.trimester',
                                      0.3061055, 0,0)
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rr.congenital.syphilis.prenatal.care.second.trimester',
                                      0.5160727, 0,0)
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rr.congenital.syphilis.prenatal.care.third.trimester',
                                      0.8182090, 0,0)

## ********** -----
SHIELD_BASE_PARAMETER_VALUES = SHIELD_BASE_PARAMETER$values



