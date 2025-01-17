# made from SHIELD_BASE_PARAMETERS.R
# what are the citation numbers?
add.parameter <- function(params, param.name,
                          value,
                          ci.lower,
                          ci.upper,
                          citation=NA,
                          comment=NA)
{
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

# *** INITIAL POPULATION INFECTED ----
# size of syphilis infected popualtion in 1940
# asusming they are all undiagnosed
# do we have data on subgroups ? 
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'proportion.initial.population.infected.syphilis',
                                      0,0,0) # TBD
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'proportion.initial.population.infected.syphilis.primary',
                                      0,0,0) # TBD
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'proportion.initial.population.infected.syphilis.secondary',
                                      0,0,0) # TBD
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'proportion.initial.population.infected.syphilis.early.latent',
                                      0,0,0) # TBD
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'proportion.initial.population.infected.syphilis.late.latent',
                                      0,0,0) # TBD
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'proportion.initial.population.infected.syphilis.tertiary',
                                      0,0,0) # TBD


# *** SYPHILIS MORTALITY ----
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
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.syphilis.mortality.congenital',
                                      0,0,0) # TBD 

# *** TRANSMISSION ----


SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER,
                                      'primary.rel.secondary.transmissibility', #observed to estimated ratio of contacts for females with male who are msm
                                      1,1,1) #?????

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER,
                                      'oe.female.pairings.with.msm', #observed to estimated ratio of contacts for females with male who are msm
                                      0.0895,0.0895*.75,0.0895*1.25, #Todd: I'm not sure what the CI should be here?
                                      citation='Pathela 2006')

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER,
                                      'fraction.heterosexual.male.pairings.with.male',
                                      0.004,0.004*.75,0.004*1.25, #Todd: I'm not sure what the CI should be here?
                                      citation='assumption')

## SEXUAL CONTACT BY RACE ----
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
## SEXUAL TRANSMISSION RATES ----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'male.to.female.sexual.transmission',
                                      4.75, 2.4, 7.1,
                                      citation=26362321)

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'female.to.male.sexual.transmission',
                                      3.75, 1.8, 5.6,
                                      citation=26362321)

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'male.to.male.sexual.transmission',
                                      5, 2.5, 7.5,
                                      citation=26362321)

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'proportion.msm.sex.with.female',
                                      0.18, 0.18*5, 0.18*2,
                                      citation=9525438)

# *** NEW BIRTHS ----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'ratio.birth.male.to.female',
                                      1.05, 1.05*5, 1.05*2,
                                      citation=00000)
# *** NATURAL HISTORY -----
## STATE DURATIONS ---- assuming as fixed 
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'duration.primary',
                                      4/52, 2/52, 6/52) #2-6weeks 
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'duration.secondary',
                                      2/12, 1/12,3/12) #1-3 months
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'duration.el',
                                      8/12, 9/12, 11/12) #9-11 months (to sum to 1year with secondary)



## TRANSITION RATES ----
# RELAPSE: 25% of persons leaving EL go to secondary, the rest go to LL
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prop.el.to.secondary',
                                      0.25,.25,.25) # Range???

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.ll.to.tertiary.male',
                                      0,0,0) # ????
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.ll.to.tertiary.female',
                                      0,0,0) # ????

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.non.ll.to.cns',
                                      0,0,0) # ????

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.ll.to.cns.male',
                                      0,0,0) # ????
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.ll.to.cns.female',
                                      0,0,0) # ????

## ASYMPTOMATIC INFECTIONS ----
## Primary: Chancre in extra-genital areas that may be missed 
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.asymptomatic.primary.msm',
                                      0.16, 0.05,.3) # 16% of primary chancre in non-penile areas
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.asymptomatic.primary.heterosexual.male',
                                      0.05,0,.1) # 5% (the range is set arbitarirly)
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.asymptomatic.primary.female',
                                      0,0,0) # ????

## Secondary: asymptomatic infection not triggering care seeking
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.asymptomatic.secondary.msm',
                                      0,0,0) # ????
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.asymptomatic.secondary.heterosexual.male',
                                      0,0,0) # ????
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.asymptomatic.secondary.female',
                                      0,0,0) # ????
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.asymptomatic.latent',
                                      1,1,1)  ## all EL/LL infections are asymothomatic (no care seeking)
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'prp.asymptomatic.tertiary.cns',
                                      0,0,0)## All teritiary & CNS infections are sympthomatic

# SYMPTOMATIC TESTING -----
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'rate.testing.symptomatic.base',
                                      1,1,1) # ????


SHIELD_BASE_PARAMETER_VALUES = SHIELD_BASE_PARAMETER$values

 
