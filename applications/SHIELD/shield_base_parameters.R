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

lb.prev=.5; hb.prev=1.5 # added uncertainty (reflecting uncertainty in fraction.reported)



# *** INITIAL POPULATION INFECTED ---- ## ----
# See input_syphilis_prev_1940.R
# proportion of initial population infected with untreated syphilis (prevalence) in 1940
# Due to unavailability of data, we etiamte this based on fraction of population diagnosed with non.congenital syphilis 
# and adjust that for fraction.reported (set to 50%) to account for cases that are undiagnosed 

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.of.initial.population.infected.syphilis',
                                      0.007,0.007*lb.prev,0.007*hb.prev,
                                      citation = "input_syphilis_prev_1940.R")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.of.infected.pop.primary',
                                      0.04486131, 0.04486131*lb, 0.04486131*hb,
                                      citation = "input_syphilis_prev_1940.R")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.of.infected.pop.secondary',
                                      0.1345839, 0.1345839*lb, 0.1345839*hb,
                                      citation = "input_syphilis_prev_1940.R")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.of.infected.pop.early.latent',
                                      0.2867137, 0.2867137*lb, 0.2867137*hb,
                                      citation = "input_syphilis_prev_1940.R")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.of.infected.pop.late.latent',
                                      0.4003808, 0.4003808*lb, 0.4003808*hb,
                                      citation = "input_syphilis_prev_1940.R")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.of.infected.pop.tertiary',
                                      0.1334603, 0.1334603*lb, 0.1334603*hb,
                                      citation = "input_syphilis_prev_1940.R")
# Diagnoses rate by stage in 1970
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'diagnoses.rate.primary.1970',
                                      2.7/100000,0,0,                                      citation = "input_syphilis_diagnoses_1970.R")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'diagnoses.rate.secondary.1970',
                                      8.1/100000,0,0,                                      citation = "input_syphilis_diagnoses_1970.R")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'diagnoses.rate.early.latent.1970',
                                      8/100000,0,0,                                      citation = "input_syphilis_diagnoses_1970.R")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'diagnoses.rate.late.latent.1970',
                                      18.525/100000,0,0,                                       citation = "input_syphilis_diagnoses_1970.R")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'diagnoses.rate.tertiary.1970',
                                      6.175/100000, 0,0,                                      citation = "input_syphilis_diagnoses_1970.R")

# # By stage (TOTAL population proportions) — derived from infected proportion × infected prevalence
# SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.of.total.pop.primary',
#                                       0.00031403, 0.00031403*lb, 0.00031403*hb,
#                                       citation = "derived from prp.of.infected.pop.primary × prp.of.initial.population.infected.syphilis")
# SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.of.total.pop.secondary',
#                                       0.00094209, 0.00094209*lb, 0.00094209*hb,
#                                       citation = "derived from prp.of.infected.pop.secondary × prp.of.initial.population.infected.syphilis")
# SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.of.total.pop.early.latent',
#                                       0.00200699, 0.00200699*lb, 0.00200699*hb,
#                                       citation = "derived from prp.of.infected.pop.early.latent × prp.of.initial.population.infected.syphilis")
# SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.of.total.pop.late.latent',
#                                       0.00280267, 0.00280267*lb, 0.00280267*hb,
#                                       citation = "derived from prp.of.infected.pop.late.latent × prp.of.initial.population.infected.syphilis")
# SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.of.total.pop.tertiary',
#                                       0.00093422, 0.00093422*lb, 0.00093422*hb,
#                                       citation = "derived from prp.of.infected.pop.tertiary × prp.of.initial.population.infected.syphilis")



# *** TRANSMISSION ---- ## ----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER,'primary.rel.secondary.transmissibility',  
                                      1,1,1)  #TBD

## ---- MIXING BY SEXUAL ORIENTATION ---- ## ----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER,
                                      'oe.female.pairings.with.msm', #observed to estimated ratio of contacts for females with male who are msm
                                      0.0895, 0.0895*lb, 0.0895*hb,  
                                      citation='Pathela 2006')

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'fraction.heterosexual_male.pairings.with.male',
                                      0.004,0.004*lb, 0.004*hb,  
                                      citation='JHEEM assumption')

## ---- SEXUAL CONTACT BY RACE ---- ## ----
#base sexual contact oes by race for the same race (black-black, hispanic-hispanic, other-other)
#these are average values from 4 different studies that are included in the pairing_input_manager
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'oe.sexual.byrace.bb',
                                      3.76, 3.76*lb, 3.76*hb, citation='JHEEMassumption')
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER,'oe.sexual.byrace.hh',
                                      2.19, 2.19*lb, 2.19*hb, citation='JHEEMassumption')
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER,'oe.sexual.byrace.oo',
                                      1.55, 1.55*lb, 1.55*hb, citation='JHEEM assumption')

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
                                      0.5,0.3,0.6,
                                      citation = "syphilis_natural_history.docx") 
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prob.vertical.transmission.mothers.late.syphilis',
                                      0.1,0.05,0.15,
                                      citation = "syphilis_natural_history.docx") 

## ---- Risk Ratios Based on Prenatal Care timing  ----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rr.congenital.syphilis.no.prenatal.care',
                                      0.36,0.28,0.449, 
                                      citation = "syphilis_natural_history.docx") 
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rr.congenital.syphilis.prenatal.care.first.trimester',
                                      0.104, 0.077, 0.14,
                                      citation = "syphilis_natural_history.docx") 
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rr.congenital.syphilis.prenatal.care.second.trimester',
                                      0.176, 0.118, 0.254,
                                      citation = "syphilis_natural_history.docx") 
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rr.congenital.syphilis.prenatal.care.third.trimester',
                                      0.406, 0.313, 0.507,
                                      citation = "syphilis_natural_history.docx") 

# *** NEW BIRTHS ---- ##----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'ratio.birth.male.to.female',
                                      1.048, 1.04, 1.06,
                                      citation = "syphilis_natural_history.docx")
## ---- Prop Multibirths -----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.births.multi.born', #proportion of births that are multibirth
                                      0.031,0.031,0.031) #we are not including the trend here
# *** NATURAL HISTORY ---- ##-----
## ---- STATE DURATIONS ---- assuming as fixed 
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'duration.primary',
                                      1/12, 0,0, #2-6weeks 
                                      citation = "syphilis_natural_history.docx")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'duration.secondary',
                                      2/12, 0,0, #1-3 months
                                      citation = "syphilis_natural_history.docx")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'duration.early.latent',
                                      9/12, 0,0, #9-11 months (to sum to 1year with secondary)
                                      citation = "syphilis_natural_history.docx")

## ---- TRANSITION RATES ----
# RELAPSE: 25% of persons leaving EL go to secondary, the rest go to LL
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prop.early.latent.to.secondary',
                                      0.25,0,0,
                                      citation = "syphilis_natural_history.docx")
# Late Latent to Tertiary:
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.late.latent.to.tertiary.male',
                                      0.01049095 ,0,0, #0.00946867 ,0.01151324,  
                                      citation = "untreated_syphilis_progression_rates.R")

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.late.latent.to.tertiary.female',
                                      0.010286933, 0,0, #0.008440789, 0.012133077,
                                      citation = "untreated_syphilis_progression_rates.R")
# Late Latent to CNS:
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.late.latent.to.cns.male',
                                      0.004465265,0,0,
                                      citation = "untreated_syphilis_progression_rates.R")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.late.latent.to.cns.female',
                                      0.002227628,0,0,
                                      citation = "untreated_syphilis_progression_rates.R")
# Early Latent to CNS:
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.early.latent.to.cns',
                                      0.085 ,0,0,# 0.05, 0.12,
                                      citation = "syphilis_natural_history.docx")

## ---- SYMPTOMATIC INFECTIONS ----                        
## Proportion of incident cases presenting with symptomatic primary or secondary disease: 
# FOR HET_MALE AND FEMALES, values set based on MSM data #LIMITATION
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.symptomatic.primary.msm.est',
                                      0.25, 0.2, 0.29, #data from MSM 
                                      citation = "syphilis_natural_history.docx")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.symptomatic.secondary.msm.est',
                                      0.16, 0.15, 0.17, #data from MSM
                                      citation = "syphilis_natural_history.docx")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.symptomatic.primary.heterosexual_male.est',
                                      0.25, 0,0,# 0.2, 0.29, 
                                      citation = "syphilis_natural_history.docx")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.symptomatic.secondary.heterosexual_male.est',
                                      0.16, 0,0, # 0.15, 0.17,  
                                      citation = "syphilis_natural_history.docx")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.symptomatic.primary.female.est',
                                      0.25, 0,0, #  0.2, 0.29,  
                                      citation = "syphilis_natural_history.docx")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.symptomatic.secondary.female.est',
                                      0.16, 0,0, #  0.15, 0.17, 
                                      citation = "syphilis_natural_history.docx")


# rate of symptomatic.testing during the tertiary stage (assuming everyone in tertiary are symptomatic)
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.testing.tertiary',
                                       12,0,0, #average of 1 month
                                      citation = "syphilis_natural_history.docx")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.testing.cns',
                                      12,0,0, #average of 1 month
                                      citation = "syphilis_natural_history.docx")


# *** TESTING ---- ##-----
## ---- HIV Testing By Age ----
# what fraction of tests reported in 15-19 agegroup are carried among 18-19 year olds
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'fraction.hiv.tests.18.19.among.15.19',
                                     0.62, 0.62*lb, 0.62*hb,
                                     citation = "input_fraction_hiv_test_by_age.R")
# *** CONTACT TRACING ---- ## ----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prop.index.cases.reached.for.contact.tracing',
                                      0.8, 0.3, 0.98,
                                      citation = "syphilis_natural_history.docx")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'contacts.diagnosed.treated.per.index.case',
                                      0.1, .05, 0.2,
                                      citation = "syphilis_natural_history.docx")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'contacts.empirically.treated.infected.per.index.case',
                                      0.1, 0.04, 0.19,
                                      citation = "syphilis_natural_history.docx")
 
#'@Todd: I am not sure about these. also if we vary them, we may add to >1
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.infected.contacts.in.primary',
                                      0.1425, 0.1425*lb,  0.1425*hb,
                                      citation = "syphilis_natural_history.docx")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.infected.contacts.in.secondary',
                                      0.4275,0.4275*lb, 0.4275*hb,
                                      citation = "syphilis_natural_history.docx")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.infected.contacts.in.early.latent',
                                      0.215,0.215*lb, 0.215*hb,
                                      citation = "syphilis_natural_history.docx")


#*** MISCLASSIFICATION ERROR **** ## -----
# source("applications/SHIELD/inputs/input_syphilis_misclassification_error.R")
percent.misclassified.el.as.llu=0.096
percent.misclassified.llu.as.el =  0.272
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'fraction.el.misclassified.ll',
                                      percent.misclassified.el.as.llu,percent.misclassified.el.as.llu*lb, percent.misclassified.el.as.llu*hb)

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'fraction.ll.misclassified.el',
                                      percent.misclassified.llu.as.el,percent.misclassified.llu.as.el*lb, percent.misclassified.llu.as.el*hb)


# TBD: *** PROP OF IMMEDIATE TREATMENTS By TESTING ROUTE ---- ##----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.treated.immediately.following.screening', 
                                      0.89,0.89*lb ,0.89*hb,
                                      citation = "syphilis_natural_history.docx")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.treated.immediately.following.testing.symptomatic', 
                                      0.89,0.89*lb ,0.89*hb,
                                      citation = "syphilis_natural_history.docx")
#differences by stage of infection (early vs late) was too small to include 

# TBD: *** RATE OF DELAYED TREATMENT ---- ##----
#if someone is diagnosed and doesnt receive immediate treatment, what is the rate of treatment
#'@Todd: should we seperate this besed on sympthoms? or for pregnant women? 
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.treatment.after.delay', 
                                      1.91,1.91*lb, 1.91*hb) #

## ********** -----
SHIELD_BASE_PARAMETER_VALUES = SHIELD_BASE_PARAMETER$values



