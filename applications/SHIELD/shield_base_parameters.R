# made from SHIELD_BASE_PARAMETERS.R

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

# *** INITIAL POPULATION INFECTED in 1970 ---- ## ----
# Due to unavailability of data, we estimate this based on rate of syphilis diagnosis in that year <input_syphilis_prev_1970.R>
# (we further add two calibration parameters to tune the number pf people with early stage and late stage syphilis in 1970) 

b.seed.infections = 1 # multiplier used to turn off infections in the demographic calibration (default=1)
# Diagnoses rate by stage in 1970
# SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'diagnoses.rate.primary.1970',
#                                       b.seed.infections* 2.7/100000,0,0)
# SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'diagnoses.rate.secondary.1970',
#                                       b.seed.infections* 8.1/100000,0,0)
# SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'diagnoses.rate.early.latent.1970',
#                                       b.seed.infections* 8/100000,0,0)
# SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'diagnoses.rate.late.latent.1970',
#                                       b.seed.infections* 18.525/100000,0,0)
# SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'diagnoses.rate.tertiary.1970',
#                                       b.seed.infections* 6.175/100000, 0,0)


#proportion of population diagnosed with syphilis (denom: total population) in 1997 - peak year
max.ps.year="1997"
prp.ps.diag.1997= SURVEILLANCE.MANAGER$data$ps.syphilis.diagnoses$estimate$cdc.sti.surveillance.reports$cdc.pdf.report$year__location[max.ps.year,'C.12580']/SURVEILLANCE.MANAGER$data$population$estimate$census.aggregated.population$census$year__location[max.ps.year,'C.12580']
prp.el.diag.1997= SURVEILLANCE.MANAGER$data$early.syphilis.diagnoses$estimate$cdc.sti.surveillance.reports$cdc.pdf.report$year__location[max.ps.year,'C.12580']/SURVEILLANCE.MANAGER$data$population$estimate$census.aggregated.population$census$year__location[max.ps.year,'C.12580']
prp.lu.diag.1997= SURVEILLANCE.MANAGER$data$unknown.duration.or.late.syphilis.diagnoses$estimate$cdc.sti.surveillance.reports$cdc.pdf.report$year__location[max.ps.year,'C.12580']/SURVEILLANCE.MANAGER$data$population$estimate$census.aggregated.population$census$year__location[max.ps.year,'C.12580']

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.ps.diag.1997',
                                      prp.ps.diag.1997,0,0)
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.el.diag.1997',
                                      prp.el.diag.1997,0,0)
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.lu.diag.1997',
                                      prp.lu.diag.1997,0,0)

# ratio of syphilis diagnoses rate in 1970 to diagnosis rate in 1990 (peak of national data)
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'ps.diagnoses.multiplier.1970',
                                      0.5,0,0)
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'el.diagnoses.multiplier.1970',
                                      0.4,0,0)
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'lu.diagnoses.multiplier.1970',
                                      2.2,0,0)

# *** INFECTIOUSNESS ---- ## ----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER,'secondary.transmissibility',  
                                      1,1,1)  # Max value
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER,'primary.rel.secondary.transmissibility',  
                                      1,1,1)  #assumption
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER,'el.rel.secondary.transmissibility',  
                                      .25,0,0)

## ---- MIXING BY SEXUAL ORIENTATION ---- ## ----
#observed to estimated ratio of contacts for females with male who are msm
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'oe.female.pairings.with.msm', 
                                      0.0895,0,0,
                                      citation='Pathela 2006')

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'fraction.heterosexual_male.pairings.with.male',
                                      0.004,0,0,
                                      citation='JHEEM assumption')

## ---- SEXUAL CONTACT BY RACE ---- ## ----
#base sexual contact oes by race for the same race (black-black, hispanic-hispanic, other-other)
#these are average values from 4 different studies that are included in the pairing_input_manager
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'oe.sexual.byrace.bb',
                                      3.76, 0,0, citation='JHEEMassumption')
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER,'oe.sexual.byrace.hh',
                                      2.19, 0,0, citation='JHEEMassumption')
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER,'oe.sexual.byrace.oo',
                                      1.55,0,0,  citation='JHEEM assumption')

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
# Boolean variable to control prenatal care as a switch
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'b.model.prenatal.care',
                                      # 1,1,1
                                      0,0,0 #'@PK:  temporary change for calibration
                                      ) 
## ---- Prob of Vertical Transmission Based on Disease Stage -----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prob.vertical.transmission.mothers.early.syphilis',
                                      # 0.5,0.3,0.6,
                                      0,0,0, #'@PK:  temporary change for calibration
                                      citation = "syphilis_natural_history.docx") 
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prob.vertical.transmission.mothers.late.syphilis',
                                      # 0.1,0.05,0.15,
                                      0,0,0, #'@PK:  temporary change for calibration
                                      citation = "syphilis_natural_history.docx") 

## ---- Risk Ratios Based on Prenatal Cares timing  ----
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
                                      4/52, 0,0, #2-6weeks 
                                      citation = "syphilis_natural_history.docx")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'duration.secondary',
                                      2/12, 0,0, #1-3 months
                                      citation = "syphilis_natural_history.docx")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'duration.early.latent',
                                      9/12, 0,0, #9-11 months (to sum to 1year with secondary)
                                      citation = "syphilis_natural_history.docx")

# Assuming a duration of 1 month for tertiary and cns: they're symptomatic stages, followed by immediate testing 
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'duration.tertiary',
                                      1/12,0,0, #average of 1 month
                                      citation = "syphilis_natural_history.docx")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'duration.cns',
                                      1/12,0,0, #average of 1 month
                                      citation = "syphilis_natural_history.docx")

## ---- TRANSITION RATES ----
# RELAPSE: 25% of persons leaving EL go to secondary, the rest go to LL
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prop.early.latent.to.secondary',
                                      # 0.25,0,0,
                                      0,0,0, #'@PK: temporary change for calibration
                                      citation = "syphilis_natural_history.docx")
# Late Latent to Tertiary:
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.late.latent.to.tertiary.male',
                                      0.01049095 ,0,0, #0.00946867 ,0.01151324,  
                                      citation = "untreated_syphilis_progression_rates.R")

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.late.latent.to.tertiary.female',
                                      0.010286933, 0,0, #0.008440789, 0.012133077,
                                      citation = "untreated_syphilis_progression_rates.R")

# Early stages to CNS: #used for primary, secondary and EL
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.early.stage.to.cns',
                                      0.085 ,0,0,# 0.05, 0.12,
                                      citation = "syphilis_natural_history.docx")

# Late Latent to CNS (by sex):
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.late.latent.to.cns.male',
                                      0.004465265,0,0,
                                      citation = "untreated_syphilis_progression_rates.R")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.late.latent.to.cns.female',
                                      0.002227628,0,0,
                                      citation = "untreated_syphilis_progression_rates.R")

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


# *** HIV TESTING ---- ##-----
# what fraction of tests reported in 15-19 agegroup are carried among 18-19 year olds
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'fraction.hiv.tests.18.19.among.15.19',
                                     0.62, 0,0,
                                     citation = "input_fraction_hiv_test_by_age.R")

# *** STI SCREENING ---- ##-----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'sti.screening.multiplier.ps',
                                      0, 0,0 #'@PK:  temporary change for calibration
                                      )
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'sti.screening.multiplier.el',
                                      1,1,1  
)
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'sti.screening.multiplier.ll',
                                      1,1,1  
)
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'sti.screening.multiplier.tertiary',
                                      1,1,1  
)
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'sti.screening.multiplier.cns',
                                      1,1,1  
)


# *** CONTACT TRACING ---- ## ----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prop.index.cases.reached.for.contact.tracing',
                                      # 0.8,0,0,   #0.3, 0.98,
                                      0,0,0, #'@PK:  temporary change for calibration
                                      citation = "syphilis_natural_history.docx")

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'contacts.diagnosed.treated.per.index.case',
                                      0.1, 0,0, #.05, 0.2,
                                      citation = "syphilis_natural_history.docx")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'contacts.empirically.treated.infected.per.index.case',
                                      0.1, 0,0, #0.04, 0.19,
                                      citation = "syphilis_natural_history.docx")
 
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.infected.contacts.in.primary',
                                      0.1425, 0,0,
                                      citation = "syphilis_natural_history.docx")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.infected.contacts.in.secondary',
                                      0.4275, 0,0,
                                      citation = "syphilis_natural_history.docx")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.infected.contacts.in.early.latent',
                                      0.215, 0,0,
                                      citation = "syphilis_natural_history.docx")


#*** MISCLASSIFICATION ERROR **** ## -----
# source("applications/SHIELD/inputs/input_syphilis_misclassification_error.R")
percent.misclassified.el.as.llu=0.096
percent.misclassified.llu.as.el =  0.272
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'fraction.el.misclassified.ll',
                                      percent.misclassified.el.as.llu,
                                      0,0)

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'fraction.ll.misclassified.el',
                                      percent.misclassified.llu.as.el,
                                      0,0)


#*** TREATMENTS INITIATION  **** ## ---- 
#*#'@PK:double check
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.treated.immediately.following.screening', 
                                      0.89,0,0,
                                      citation = "syphilis_natural_history.docx")
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.treated.immediately.following.testing.symptomatic', 
                                      0.89,0,0,
                                      citation = "syphilis_natural_history.docx")
#differences by stage of infection (early vs late) was too small to include 

#if someone is diagnosed and doesn't receive immediate treatment, what is the rate of treatment
#'@Todd: should we separate this based on symptoms? or for pregnant women? 
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.treatment.after.delay', 
                                      1.91,0,0
                                      ) 

## ********** -----
SHIELD_BASE_PARAMETER_VALUES = SHIELD_BASE_PARAMETER$values



