# made from SHIELD_BASE_PARAMETERS.R

lb=hb=1 #we use these multipliers to construct a range for those parameters where only a single value is available 

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
# citation numbers are oubmed ID, they're for our own records'

# *** INITIAL POPULATION INFECTED ---- ## ----
# size of syphilis infected popualtion in 1940
# asusming they are all undiagnosed # do we have data on subgroups ? 
# proportion of ininitial population infected with untreated syphilis (prevalence)
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.of.initial.population.infected.syphilis',
                                      0,0,0) # TBD #age, race, sex
#by stage:
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.of.infected.pop.primary',
                                      0,0,0) # TBD
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.of.infected.pop.secondary',
                                      0,0,0) # TBD
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.of.infected.pop.early.latent',
                                      0,0,0) # TBD
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.of.infected.pop.late.latent',
                                      0,0,0) # TBD
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.of.infected.pop.tertiary',
                                      0,0,0) # TBD


# *** SYPHILIS MORTALITY ---- ## ----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.syphilis.mortality.primary',
                                      0,0,0) # TBD
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.syphilis.mortality.secondary',
                                      0,0,0) # TBD
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.syphilis.mortality.early.latent',
                                      0,0,0) # TBD
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.syphilis.mortality.late.latent',
                                      0,0,0) # TBD
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.syphilis.mortality.tertiary',
                                      0,0,0) # TBD
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.syphilis.mortality.cns',
                                      0,0,0) # TBD


# *** TRANSMISSION ---- ## ----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER,
                                      'primary.rel.secondary.transmissibility',  
                                      1,1,1)  
## ---- MIXING BY SEXUAL ORIENTATION ---- ## ----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER,
                                      'oe.female.pairings.with.msm', #observed to estimated ratio of contacts for females with male who are msm
                                      0.0895,0.0895*.75,0.0895*1.25, #Todd: I'm not sure what the CI should be here?
                                      citation='Pathela 2006')

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER,
                                      'fraction.heterosexual_male.pairings.with.male',
                                      0.004,0.004*.75,0.004*1.25, #Todd: I'm not sure what the CI should be here?
                                      citation='assumption')

## ---- SEXUAL CONTACT BY RACE ---- ## ----
#base sexual contact oes by race for the same race (black-black, hispanic-hispanic, other-other)
#these are average values from 4 different studies that are included in the pairing_input_manager
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER,
                                      'oe.sexual.byrace.bb',
                                      3.76, 3.76*.75, 3.76*1.25,
                                      citation='assumption')
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER,
                                      'oe.sexual.byrace.hh',
                                      2.19, 2.19*.75, 2.19*1.25,
                                      citation='assumption')
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER,
                                      'oe.sexual.byrace.oo',
                                      1.55, 1.55*.75, 1.55*1.25,
                                      citation='assumption')
## ---- SEXUAL TRANSMISSION RATES ---- ##----
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

# *** CONGENITAL SYPHILIS ---- ##----
## ---- Prob of Vertical Transmission Based on Disease Stage -----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prob.vertical.transmission.mothers.early.syphilis',
                                      0.5,0.3,0.6) #
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prob.vertical.transmission.mothers.late.syphilis',
                                      0.1,0.05,0.15) #

## ---- Risk Ratios Based on Prenatal Care timing  ----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rr.congenital.syphilis.no.prenatal.care',
                                      1,1,1) #
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rr.congenital.syphilis.prenatal.care.first.trimester',
                                      0.104, 0.077, 0.14) #
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rr.congenital.syphilis.prenatal.care.second.trimester',
                                      0.176, 0.118, 0.254) #
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rr.congenital.syphilis.prenatal.care.third.trimester',
                                      0.406, 0.313, 0.507) #

# *** NEW BIRTHS ---- ##----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'ratio.birth.male.to.female',
                                      1.05, 1.05*5, 1.05*2)
## ---- Prop Multibirths -----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.births.multi.born', #proportion of births that are multibirth
                                      0.031,0.031,0.031) # 
# *** NATURAL HISTORY ---- ##-----
## ---- STATE DURATIONS ---- assuming as fixed 
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'duration.primary',
                                      4/52, 2/52, 6/52) #2-6weeks 
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'duration.secondary',
                                      2/12, 1/12,3/12) #1-3 months
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'duration.early.latent',
                                      8/12, 9/12, 11/12) #9-11 months (to sum to 1year with secondary)

## ---- TRANSITION RATES ----
# RELAPSE: 25% of persons leaving EL go to secondary, the rest go to LL
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prop.early.latent.to.secondary',
                                      0.25,.25 * lb,.25 *hb) # Range??? 
# Late Latent to Tertiary:
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.late.latent.to.tertiary.male',
                                      0.01049095 ,0.00946867 ,0.01151324)  #untreated_syphilis_progression_rates.R

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.late.latent.to.tertiary.female',
                                      0.010286933, 0.008440789, 0.012133077) #untreated_syphilis_progression_rates.R
# Late Latent to CNS:
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.late.latent.to.cns.male',
                                      0.004465265,0.004465265* lb,0.004465265*hb)
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.late.latent.to.cns.female',
                                      0.002227628,0.002227628 * lb,0.002227628* hb) 
# Early Latent to CNS:
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.early.latent.to.cns',
                                      0.085 ,0.05, 0.12) 

## ---- ASYMPTOMATIC INFECTIONS ----
## Primary: Chancre in extra-genital areas that may be missed 
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.asymptomatic.primary.msm',
                                      0.16, 0.05,.3) # 16% of primary chancre in non-penile areas
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.asymptomatic.primary.heterosexual_male',
                                      0.05,0,.1) # 5% (the range is set arbitarirly)
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.asymptomatic.primary.female',
                                      0,0,0) # ????

## Secondary: asymptomatic infection not triggering care seeking
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.asymptomatic.secondary.msm',
                                      0,0,0) # ????
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.asymptomatic.secondary.heterosexual_male',
                                      0,0,0) # ????
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.asymptomatic.secondary.female',
                                      0,0,0) # ????
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.asymptomatic.latent',
                                      1,1,1)  ## all EL/LL infections are asymothomatic (no care seeking)
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.asymptomatic.tertiary.cns',
                                      0,0,0)## All teritiary & CNS infections are sympthomatic

# *** TESTING ---- ##-----
## --- Symptomatic testing ----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.testing.symptomatic.base',
                                      1,1,1) # ????

## ---- HIV Testing By Age ----
# what fraction of tests performed in 15-19 year olds are among 18-19 year olds
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'fraction.hiv.tests.18.19.among.15.19',
                                      0.9,.9,.9) # #'PK: to estimate from CDC data

# *** PROP OF IMMEDIATE TREATMENTS By TESTING ROUTE ---- ##----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.treated.immediately.following.screening', 
                                      0,0,0) #
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.treated.immediately.following.testing.symptomatic', 
                                      0,0,0) #
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.treated.immediately.following.prenatal.care', 
                                      0,0,0) #
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.treated.immediately.following.contact.tracing', 
                                      0,0,0) #

# *** RATE OF DELAYED TREATMENT ---- ##----
#if someone is diagnosed and doesnt receive immediate treatment, what is the rate of treatment
#'@TODO: should we seperate this besed on sympthoms? or for pregnant women? 
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.treatment.after.delay', 
                                      .1,.1,.1) #


# *** CONTACT TRACING ---- ## ----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prop.index.cases.reached.for.contact.tracing',
                                      0.8, 0.5, 0.98)  
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'contacts.diagnosed.treated.per.index.case',
                                      0.1, .05, 0.17)
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'contacts.empirically.treated.per.index.case',
                                      0.25, 0.12, 0.6)
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.of.empirically.treated.contacts.with.syphilis',
                                      0.4, 0.2, 0.6)
#'@Todd: I am not sure about these. also if we vary them, we may add to >1
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.infected.contacts.in.primary',
                                      0.25,0.25*lb, 0.25*hb)
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.infected.contacts.in.secondary',
                                      0.25,0.25*lb, 0.25*hb)
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.infected.contacts.in.early.latent',
                                      0.25,0.25*lb, 0.25*hb)


SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'fraction.el.misclassified.ll',
                                      0.25,0.25*lb, 0.25*hb)

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'fraction.ll.misclassified.el',
                                      0.25,0.25*lb, 0.25*hb)

## ********** -----
SHIELD_BASE_PARAMETER_VALUES = SHIELD_BASE_PARAMETER$values



