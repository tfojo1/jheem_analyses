b.INFECTIOUSNESS=FALSE
b.RELAPSE=FALSE
b.PS.SCREENING=FALSE
b.CONTACT.TRACING=FALSE

source("applications/SHIELD/shield_base_parameters.R")

# Helpul command: #get.intervals(variable name): Get intervals (confidence/credible intervals) for the variables in a distribution

logit = function(p){
  log(p) - log(1-p)
}

#my best guess for this parameter is different in different locations, so we formulate prior as a multiply of the best guess
# Defining the calibration parameters and prior distributions

#1- PARAMETER PRIORS:----

## POPULATION.PARAMETERS.PRIOR ----
POPULATION.PARAMETERS.PRIOR=join.distributions( 
  ## Fertility rates ----
  # (6 agegroups, 3 race, 2 knots)-> max 36 params
  # we start with 6 age, and 3 race, parameters applied to both knots -> 9 total
  # Race-level fertility rate multipliers
  black.fertility.rate.multiplier    = Lognormal.Distribution(meanlog = 0, sdlog = 0.5 * log(2)),
  hispanic.fertility.rate.multiplier = Lognormal.Distribution(meanlog = 0, sdlog = 0.5 * log(2)),
  other.fertility.rate.multiplier    = Lognormal.Distribution(meanlog = 0, sdlog = 0.5 * log(2)),
  # Age-level fertility rate multipliers
  age15.19.fertility.rate.multiplier = Lognormal.Distribution(meanlog = 0, sdlog = 0.5 * log(2)),
  age20.24.fertility.rate.multiplier = Lognormal.Distribution(meanlog = 0, sdlog = 0.5 * log(2)),
  age25.29.fertility.rate.multiplier = Lognormal.Distribution(meanlog = 0, sdlog = 0.5 * log(2)),
  age30.34.fertility.rate.multiplier = Lognormal.Distribution(meanlog = 0, sdlog = 0.5 * log(2)),
  age35.39.fertility.rate.multiplier = Lognormal.Distribution(meanlog = 0, sdlog = 0.5 * log(2)),
  age40.44.fertility.rate.multiplier = Lognormal.Distribution(meanlog = 0, sdlog = 0.5 * log(2)),
  
  # Mortality rates ----
  # By Race:
  black.general.mortality.rate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  hispanic.general.mortality.rate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  other.general.mortality.rate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  # by Sex:
  male.general.mortality.rate.multiplier = Lognormal.Distribution(meanlog = 0,sdlog = 0.5*log(2)), 
  female.general.mortality.rate.multiplier = Lognormal.Distribution(meanlog = 0,sdlog = 0.5*log(2)),
  
  # Immigration by race ----
  black.immigration.rate.multiplier.1= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  hispanic.immigration.rate.multiplier.1= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  other.immigration.rate.multiplier.1= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  black.immigration.rate.multiplier.2= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  hispanic.immigration.rate.multiplier.2= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  other.immigration.rate.multiplier.2= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2))
)
## AGING.PARAMETERS.PRIOR ----
AGING.PARAMETERS.PRIOR=join.distributions( 
  # By age, race, sex for 2 knots:
  age14.black.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age19.black.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age24.black.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age29.black.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age34.black.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age39.black.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age44.black.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age49.black.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age54.black.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age64.black.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  #
  age14.hispanic.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age19.hispanic.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age24.hispanic.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age29.hispanic.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age34.hispanic.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age39.hispanic.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age44.hispanic.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age49.hispanic.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age54.hispanic.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age64.hispanic.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  #
  age14.other.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age19.other.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age24.other.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age29.other.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age34.other.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age39.other.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age44.other.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age49.other.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age54.other.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age64.other.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  
  age14.black.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age19.black.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age24.black.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age29.black.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age34.black.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age39.black.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age44.black.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age49.black.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age54.black.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age64.black.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  #
  age14.hispanic.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age19.hispanic.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age24.hispanic.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age29.hispanic.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age34.hispanic.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age39.hispanic.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age44.hispanic.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age49.hispanic.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age54.hispanic.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age64.hispanic.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  #
  age14.other.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age19.other.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age24.other.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age29.other.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age34.other.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age39.other.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age44.other.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age49.other.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age54.other.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age64.other.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2))
)

## TRANSMISSION.PARAMETERS.PRIOR ----
TRANSMISSION.PARAMETERS.PRIOR=join.distributions( 
  # Initial infection multipliers:
  # initial.infection.multiplier.1970.early = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)), #ps and EL stages
  # initial.infection.multiplier.1970.late = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)), #ll and tertiary
  # 
  # multipliers in 1970 (relative diagnoses in 1970 to the peak diagnoses between 1993-99)
  ps.diagnoses.multiplier.1970 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)), 
  el.diagnoses.multiplier.1970 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)), 
  lu.diagnoses.multiplier.1970 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)), 
  
  ## Transmission
  global.transmission.rate = Lognormal.Distribution(meanlog = log(3.5), sdlog = 0.5*log(2)), #directly used in specification (will need sth uch larger) 
  
  #12 independant params
  # msm multipliers by time
  transmission.rate.multiplier.msm1970 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  transmission.rate.multiplier.msm1990 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  transmission.rate.multiplier.msm1995 = Lognormal.Distribution(meanlog = log(1.2), sdlog = 0.5*log(2)),#1995 #increasing the peak value
  transmission.rate.multiplier.msm2000 = Lognormal.Distribution(meanlog = log(0.9), sdlog = 0.5*log(2)), 
  transmission.rate.multiplier.msm2010 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)), 
  transmission.rate.multiplier.msm2020 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)), 
  
  # heterosexual multipliers by time
  transmission.rate.multiplier.heterosexual1970 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  transmission.rate.multiplier.heterosexual1990 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  transmission.rate.multiplier.heterosexual1995 = Lognormal.Distribution(meanlog = log(1.2), sdlog = 0.5*log(2)),#1995 #increasing the peak value
  transmission.rate.multiplier.heterosexual2000 = Lognormal.Distribution(meanlog = log(0.9), sdlog = 0.5*log(2)), 
  transmission.rate.multiplier.heterosexual2010 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)), 
  transmission.rate.multiplier.heterosexual2020 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)), 
  
  ### race multipliers (shared for msm and het):
  transmission.rate.multiplier.black= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  transmission.rate.multiplier.hispanic= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  transmission.rate.multiplier.other= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  
  ## Sexual Mixing by Age
  age.mixing.sd.mult = Lognormal.Distribution(0, 0.25*log(2)) #directly used in specification helper function
  #to control the standard deviation of the contact matrix by age
  
  
) 


TESTING.PARAMETERS.PRIOR=join.distributions( 
  # for symptomatic testing
  prp.symptomatic.primary.msm = Logitnormal.Distribution( meanlogit = logit(SHIELD_BASE_PARAMETER_VALUES['prp.symptomatic.primary.msm.est']), sdlogit = log(2)/2 ) , 
  prp.symptomatic.primary.heterosexual_male = Logitnormal.Distribution( meanlogit = logit(SHIELD_BASE_PARAMETER_VALUES['prp.symptomatic.primary.heterosexual_male.est']), sdlogit = log(2)/2 ) ,
  prp.symptomatic.primary.female = Logitnormal.Distribution( meanlogit = logit(SHIELD_BASE_PARAMETER_VALUES['prp.symptomatic.primary.female.est']), sdlogit = log(2)/2 ) ,
  prp.symptomatic.secondary.msm = Logitnormal.Distribution( meanlogit = logit(SHIELD_BASE_PARAMETER_VALUES['prp.symptomatic.secondary.msm.est']), sdlogit = log(2)/2 ) ,
  prp.symptomatic.secondary.heterosexual_male = Logitnormal.Distribution( meanlogit = logit(SHIELD_BASE_PARAMETER_VALUES['prp.symptomatic.secondary.heterosexual_male.est']), sdlogit = log(2)/2 ) ,
  prp.symptomatic.secondary.female= Logitnormal.Distribution( meanlogit = logit(SHIELD_BASE_PARAMETER_VALUES['prp.symptomatic.secondary.female.est']), sdlogit = log(2)/2 ) ,
  
  # for HIV screening
  hiv.testing.or = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  hiv.testing.slope.or = Lognormal.Distribution(meanlog = 0, sdlog = (log(2)/2)/5),
  
  # STI screening knots multiplier (relative to HIV screening)
  syphilis.screening.multiplier.1980 = Lognormal.Distribution(meanlog = 0, sdlog = (log(2)/2)),
  syphilis.screening.multiplier.1990 = Lognormal.Distribution(meanlog = 0, sdlog = (log(2)/2)),
  syphilis.screening.multiplier.2000 = Lognormal.Distribution(meanlog = 0, sdlog = (log(2)/2)),
  syphilis.screening.multiplier.2010 = Lognormal.Distribution(meanlog = 0, sdlog = (log(2)/2)),
  syphilis.screening.multiplier.2020 = Lognormal.Distribution(meanlog = 0, sdlog = (log(2)/2)),
  
  # STI screening multiplier by stage (defined in specification-no linking needed here)
  
  # b.PS.SCREENING
  sti.screening.multiplier.ps = Lognormal.Distribution(meanlog = log(.5), sdlog = log(2)), #get.intervals(sti.screening.multiplier.ps) #most values between 0.25-0.75
  
  sti.screening.multiplier.el = Lognormal.Distribution(meanlog = log(3), sdlog = 0.75 *log(2)), #changing the prior to reflect higher freq of screening among syphilis-infected subgroups (highrisk)
  sti.screening.multiplier.ll = Lognormal.Distribution(meanlog = log(3), sdlog = 0.75 *log(2)),
  sti.screening.multiplier.tertiary = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  sti.screening.multiplier.cns = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  
  # Contact tracing
  # b.CONTACT.TRACING
  # prop.index.cases.reached.for.contact.tracing = 0.8 [0.3, 0.98] #I chose the sdlogit to roughly create this range
  prop.index.cases.reached.for.contact.tracing=Logitnormal.Distribution(meanlogit = logit(.8), sdlogit = log(2)*1.7 )# get.intervals(prop.index.cases.reached.for.contact.tracing)
)
# x=Lognormal.Distribution(meanlog = log(1), sdlog = 0.5*log(2))
# get.intervals(x)
# calculate.density(x,10)/calculate.density(x,1) #penalty for drawing a point at 10 instead of 1
# 
# x=Lognormal.Distribution(meanlog = log(3), sdlog = 0.5*log(2))
# get.intervals(x)
# calculate.density(x,10)/calculate.density(x,3)
# 
# x=Lognormal.Distribution(meanlog = log(3), sdlog = log(2))
# get.intervals(x)
# calculate.density(x,10)/calculate.density(x,3)

## SYPHILIS.PARAMETERS.PRIOR ----
# SYPHILIS.PARAMETERS.PRIOR=join.distributions( 
#   ## Transitions:
#   # rate.el.to.secondary.mult = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)), 
#   #   # rate.ll.to.tertiary.mult = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)), 
#   # 
#   # # to CNS:
#   # #should differentiate by sex?
#   # rate.primary.to.cns.mult = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)), 
#   # rate.secondary.to.cns.mult = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)), 
#   # rate.el.to.cns.mult = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)), 
#   # rate.ll.to.cns.mult = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)), 
#  
#   
#   
# ) 


#2- LINKING PARAMETERS TO FUNCTIONAL FORMS....  -----
SHIELD.APPLY.PARAMETERS.FN = function(model.settings, parameters ){ 
  ages=model.settings$specification.metadata$dim.names$age
  sexes=model.settings$specification.metadata$dim.names$sex
  races=model.settings$specification.metadata$dim.names$race
  
  fertile.ages=model.settings$specification.metadata$dim.names$age[2:7]
  fertile.age.ranges= c("15.19","20.24","25.29","30.34","35.39","40.44") 
  #buckets of aging from:
  q=model.settings$specification.metadata$age.upper.bounds
  aging.from=q[1: (length(q)-1)]-1
  
  ## Aging Rates ----
  #10 (ages) * 3 (races) * 3 sexes= 90 for 2 knots = 180
  for(i in c(1,2)){ #spline with 2 knots
    for(age.index in 1:length(aging.from)) {
      for(race in races){
        # for(s in sexes){
        agegroup=ages[age.index]
        paramName=paste0("age",aging.from[age.index],".",race,".aging.rate.multiplier.",i)
        set.element.functional.form.interaction.alphas(model.settings,
                                                       element.name = "rate.general.aging",
                                                       alpha.name = paste0("time",i),
                                                       value = parameters[paramName],
                                                       applies.to.dimension.values =list(age = agegroup, race = race))
      }}}                                      
  
  ## Fertility rates by race/age to time1/time2 knots----
  # Fertility multipliers by race (applied to both time1 and time2)
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "fertility.rate",
                                                 alpha.name = "time1",
                                                 values = parameters[paste0(races, ".fertility.rate.multiplier")],
                                                 dimension = "race",
                                                 applies.to.dimension.values = races)
  
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "fertility.rate",
                                                 alpha.name = "time2",
                                                 values = parameters[paste0(races, ".fertility.rate.multiplier")],
                                                 dimension = "race",
                                                 applies.to.dimension.values = races)
  
  # Fertility multipliers by age ----
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "fertility.rate",
                                                 alpha.name = "time1",
                                                 values = parameters[paste0("age", fertile.age.ranges, ".fertility.rate.multiplier")],
                                                 dimension = "age",
                                                 applies.to.dimension.values = fertile.ages)
  
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "fertility.rate",
                                                 alpha.name = "time2",
                                                 values = parameters[paste0("age", fertile.age.ranges, ".fertility.rate.multiplier")],
                                                 dimension = "age",
                                                 applies.to.dimension.values = fertile.ages)
  
  
  
  ## Immigration rate multipliers by race for time1/time2 knots ----
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "rate.immigration",
                                                 alpha.name = "time.1",
                                                 values = parameters[paste0(races,".immigration.rate.multiplier.1")],
                                                 dimension = "race",
                                                 applies.to.dimension.values = races)
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "rate.immigration",
                                                 alpha.name = "time.2",
                                                 values = parameters[paste0(races,".immigration.rate.multiplier.2")],
                                                 dimension = "race",
                                                 applies.to.dimension.values = races)
  
  ## Emigration coefficients  by race for time1/time2 knots ----
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "rate.emigration",
                                                 alpha.name = "time.1",
                                                 values = 1/parameters[paste0(races,".immigration.rate.multiplier.1")],
                                                 dimension = "race",
                                                 applies.to.dimension.values = races)
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "rate.emigration",
                                                 alpha.name = "time.2",
                                                 values = 1/parameters[paste0(races,".immigration.rate.multiplier.2")],
                                                 dimension = "race",
                                                 applies.to.dimension.values = races)
  ## Mortality rates by race ----
  races=model.settings$specification.metadata$dim.names$race
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "rate.general.mortality",
                                                 alpha.name = 'value',
                                                 values = parameters[paste0(races,".general.mortality.rate.multiplier")],
                                                 dimension = "race",
                                                 applies.to.dimension.values = races)
  ## Mortality rates by sex ----
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "rate.general.mortality",
                                                 alpha.name = 'value',
                                                 values = parameters["male.general.mortality.rate.multiplier"],
                                                 dimension = "sex",
                                                 applies.to.dimension.values = c('heterosexual_male','msm'))
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "rate.general.mortality",
                                                 alpha.name = 'value',
                                                 values = parameters["female.general.mortality.rate.multiplier"],
                                                 dimension = "sex",
                                                 applies.to.dimension.values = c('female'))
  
  ## Transmission ----
  for(year in c(1970,1990,1995,2000,2010,2020)){
    #multipliers for msm rates in each knot:
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "transmission.rate.msm",
                                                   alpha.name = paste0(year),
                                                   values = parameters[paste0("transmission.rate.multiplier.msm",year)],
                                                   dimension = 'all',
                                                   applies.to.dimension.values = 'all')
    #multipliers for heterosexual rates in each knot:
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "transmission.rate.heterosexual",
                                                   alpha.name = paste0(year),
                                                   values = parameters[paste0("transmission.rate.multiplier.heterosexual",year)],
                                                   dimension = 'all',
                                                   applies.to.dimension.values = 'all')
    
    #race multipliers, shared for msm and heterosexuals: 
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "transmission.rate.msm",
                                                   alpha.name = paste0(year),
                                                   values = parameters[c("transmission.rate.multiplier.black","transmission.rate.multiplier.hispanic", "transmission.rate.multiplier.other")],
                                                   dimension = "race.to", #recipient
                                                   applies.to.dimension.values = c("black","hispanic", "other"))
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "transmission.rate.heterosexual",
                                                   alpha.name = paste0(year),
                                                   values = parameters[c("transmission.rate.multiplier.black","transmission.rate.multiplier.hispanic", "transmission.rate.multiplier.other")],
                                                   dimension = "race.to", #recipient
                                                   applies.to.dimension.values = c("black","hispanic", "other"))
    
  }
  
  
  
  ## STI SCREENING  ----
  # Changing the intercept and slope for HIV tests
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "rate.testing.hiv.without.covid.over.14",
                                                 alpha.name = "intercept",
                                                 values = parameters["hiv.testing.or"],
                                                 dimension = "all", #recipient
                                                 applies.to.dimension.values = "all")
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "rate.testing.hiv.without.covid.over.14",
                                                 alpha.name = "slope",
                                                 values = parameters["hiv.testing.slope.or"],
                                                 dimension = "all", #recipient
                                                 applies.to.dimension.values = "all")
  
  # Changing the knot values for retio of STI screening to HIV tests
  for(time in c("1980","1990","2000","2010","2020")){
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "multiplier.syphilis.screening.to.hiv.tests",
                                                   alpha.name = time,
                                                   values = parameters[paste0("syphilis.screening.multiplier.",time)],
                                                   dimension = "all", #recipient
                                                   applies.to.dimension.values = "all")
  }
  
  
}



#3- SAMPLING BLOCKS: ----
# classic mcmc samples one param at a time, adaptive mcms samples multiple params
#could add more params here (1-5 per block)
## SHIELD.POPULATION.SAMPLING.BLOCKS ----
SHIELD.POPULATION.SAMPLING.BLOCKS = list(
  # Fertility ---- 
  fertility.rates.by.race = c(
    "black.fertility.rate.multiplier",
    "hispanic.fertility.rate.multiplier",
    "other.fertility.rate.multiplier"
  ),
  fertility.rates.by.age.1 = c(
    "age15.19.fertility.rate.multiplier",
    "age20.24.fertility.rate.multiplier",
    "age25.29.fertility.rate.multiplier"
  ),
  fertility.rates.by.age.2 = c(
    "age30.34.fertility.rate.multiplier",
    "age35.39.fertility.rate.multiplier",
    "age40.44.fertility.rate.multiplier"
  ),
  #Mortality ----
  mortality.rates.by.race=c("black.general.mortality.rate.multiplier",
                            "hispanic.general.mortality.rate.multiplier",
                            "other.general.mortality.rate.multiplier"),
  moratlity.rates.by.sex=c("male.general.mortality.rate.multiplier",
                           "female.general.mortality.rate.multiplier"),
  #Immigration ----
  black.immigration = c("black.immigration.rate.multiplier.1",
                        "black.immigration.rate.multiplier.2"),
  hispanic.immigration = c("hispanic.immigration.rate.multiplier.1",
                           "hispanic.immigration.rate.multiplier.2"),
  other.immigration = c("other.immigration.rate.multiplier.1",
                        "other.immigration.rate.multiplier.2")
)
## SHIELD.AGING.SAMPLING.BLOCKS ---- 
SHIELD.AGING.SAMPLING.BLOCKS = list(
  aging.black.group1=c(
    "age14.black.aging.rate.multiplier.1",
    "age14.black.aging.rate.multiplier.2",
    "age19.black.aging.rate.multiplier.1",
    "age19.black.aging.rate.multiplier.2"),
  aging.black.group2=c(
    "age19.black.aging.rate.multiplier.1",
    "age19.black.aging.rate.multiplier.2",
    "age24.black.aging.rate.multiplier.1",
    "age24.black.aging.rate.multiplier.2"  ),
  aging.black.group3=c(
    "age24.black.aging.rate.multiplier.1",
    "age24.black.aging.rate.multiplier.2" ,
    "age29.black.aging.rate.multiplier.1",
    "age29.black.aging.rate.multiplier.2"),
  aging.black.group4=c(
    "age29.black.aging.rate.multiplier.1",
    "age29.black.aging.rate.multiplier.2",
    "age34.black.aging.rate.multiplier.1",
    "age34.black.aging.rate.multiplier.2" ),
  aging.black.group5=c(
    "age34.black.aging.rate.multiplier.1",
    "age34.black.aging.rate.multiplier.2",
    "age39.black.aging.rate.multiplier.1",
    "age39.black.aging.rate.multiplier.2"),
  aging.black.group6=c(
    "age39.black.aging.rate.multiplier.1",
    "age39.black.aging.rate.multiplier.2",
    "age44.black.aging.rate.multiplier.1",
    "age44.black.aging.rate.multiplier.2"),
  aging.black.group7=c(
    "age44.black.aging.rate.multiplier.1",
    "age44.black.aging.rate.multiplier.2",
    "age49.black.aging.rate.multiplier.1",
    "age49.black.aging.rate.multiplier.2"),
  aging.black.group8=c(
    "age49.black.aging.rate.multiplier.1",
    "age49.black.aging.rate.multiplier.2",
    "age54.black.aging.rate.multiplier.1",
    "age54.black.aging.rate.multiplier.2"),
  aging.black.group9=c(
    "age64.black.aging.rate.multiplier.1",
    "age64.black.aging.rate.multiplier.2"),
  
  ###
  aging.hispanic.group1=c(
    "age14.hispanic.aging.rate.multiplier.1",
    "age14.hispanic.aging.rate.multiplier.2",
    "age19.hispanic.aging.rate.multiplier.1",
    "age19.hispanic.aging.rate.multiplier.2"),
  aging.hispanic.group2=c(
    "age19.hispanic.aging.rate.multiplier.1",
    "age19.hispanic.aging.rate.multiplier.2",
    "age24.hispanic.aging.rate.multiplier.1",
    "age24.hispanic.aging.rate.multiplier.2"  ),
  aging.hispanic.group3=c(
    "age24.hispanic.aging.rate.multiplier.1",
    "age24.hispanic.aging.rate.multiplier.2" ,
    "age29.hispanic.aging.rate.multiplier.1",
    "age29.hispanic.aging.rate.multiplier.2"),
  aging.hispanic.group4=c(
    "age29.hispanic.aging.rate.multiplier.1",
    "age29.hispanic.aging.rate.multiplier.2",
    "age34.hispanic.aging.rate.multiplier.1",
    "age34.hispanic.aging.rate.multiplier.2" ),
  aging.hispanic.group5=c(
    "age34.hispanic.aging.rate.multiplier.1",
    "age34.hispanic.aging.rate.multiplier.2",
    "age39.hispanic.aging.rate.multiplier.1",
    "age39.hispanic.aging.rate.multiplier.2"),
  aging.hispanic.group6=c(
    "age39.hispanic.aging.rate.multiplier.1",
    "age39.hispanic.aging.rate.multiplier.2",
    "age44.hispanic.aging.rate.multiplier.1",
    "age44.hispanic.aging.rate.multiplier.2"),
  aging.hispanic.group7=c(
    "age44.hispanic.aging.rate.multiplier.1",
    "age44.hispanic.aging.rate.multiplier.2",
    "age49.hispanic.aging.rate.multiplier.1",
    "age49.hispanic.aging.rate.multiplier.2"),
  aging.hispanic.group8=c(
    "age49.hispanic.aging.rate.multiplier.1",
    "age49.hispanic.aging.rate.multiplier.2",
    "age54.hispanic.aging.rate.multiplier.1",
    "age54.hispanic.aging.rate.multiplier.2"),
  aging.hispanic.group9=c(
    "age64.hispanic.aging.rate.multiplier.1",
    "age64.hispanic.aging.rate.multiplier.2"),
  
  ##
  aging.other.group1=c(
    "age14.other.aging.rate.multiplier.1",
    "age14.other.aging.rate.multiplier.2",
    "age19.other.aging.rate.multiplier.1",
    "age19.other.aging.rate.multiplier.2"),
  aging.other.group2=c(
    "age19.other.aging.rate.multiplier.1",
    "age19.other.aging.rate.multiplier.2",
    "age24.other.aging.rate.multiplier.1",
    "age24.other.aging.rate.multiplier.2"  ),
  aging.other.group3=c(
    "age24.other.aging.rate.multiplier.1",
    "age24.other.aging.rate.multiplier.2" ,
    "age29.other.aging.rate.multiplier.1",
    "age29.other.aging.rate.multiplier.2"),
  aging.other.group4=c(
    "age29.other.aging.rate.multiplier.1",
    "age29.other.aging.rate.multiplier.2",
    "age34.other.aging.rate.multiplier.1",
    "age34.other.aging.rate.multiplier.2" ),
  aging.other.group5=c(
    "age34.other.aging.rate.multiplier.1",
    "age34.other.aging.rate.multiplier.2",
    "age39.other.aging.rate.multiplier.1",
    "age39.other.aging.rate.multiplier.2"),
  aging.other.group6=c(
    "age39.other.aging.rate.multiplier.1",
    "age39.other.aging.rate.multiplier.2",
    "age44.other.aging.rate.multiplier.1",
    "age44.other.aging.rate.multiplier.2"),
  aging.other.group7=c(
    "age44.other.aging.rate.multiplier.1",
    "age44.other.aging.rate.multiplier.2",
    "age49.other.aging.rate.multiplier.1",
    "age49.other.aging.rate.multiplier.2"),
  aging.other.group8=c(
    "age49.other.aging.rate.multiplier.1",
    "age49.other.aging.rate.multiplier.2",
    "age54.other.aging.rate.multiplier.1",
    "age54.other.aging.rate.multiplier.2"),
  aging.other.group9=c(
    "age64.other.aging.rate.multiplier.1",
    "age64.other.aging.rate.multiplier.2")
)

## SHIELD.TRANSMISSION.SAMPLING.BLOCKS ----
SHIELD.TRANSMISSION.SAMPLING.BLOCKS = list(
  # initial.infections = c(
  #   "initial.infection.multiplier.1970.early",
  #   "initial.infection.multiplier.1970.late"), 
  #
  initial.infections=c(
    'ps.diagnoses.multiplier.1970', 
    'el.diagnoses.multiplier.1970', 
    'lu.diagnoses.multiplier.1970'
  ),
  global.transmission.rate=c("global.transmission.rate"),
  #
  msm.transmission.block1 = c(
    "transmission.rate.multiplier.msm1970",
    "transmission.rate.multiplier.msm1990",
    "transmission.rate.multiplier.msm1995"),
  msm.transmission.block2=c(
    "transmission.rate.multiplier.msm2000",
    "transmission.rate.multiplier.msm2010",
    "transmission.rate.multiplier.msm2020"),
  #
  het.transmission.block1 =c(
    "transmission.rate.multiplier.heterosexual1970",
    "transmission.rate.multiplier.heterosexual1990",
    "transmission.rate.multiplier.heterosexual1995"),
  het.transmission.block2=c(
    "transmission.rate.multiplier.heterosexual2000",
    "transmission.rate.multiplier.heterosexual2010",
    "transmission.rate.multiplier.heterosexual2020"),
  race.transmission = c(
    "transmission.rate.multiplier.black",
    "transmission.rate.multiplier.hispanic",
    "transmission.rate.multiplier.other"
  ),
  age.mixing.transmission=("age.mixing.sd.mult")
)


SHIELD.TESTING.SAMPLING.BLOCKS = list(
  symptomatic.primary = c(
    "prp.symptomatic.primary.msm",
    "prp.symptomatic.primary.heterosexual_male",
    "prp.symptomatic.primary.female"
  ),
  symptomatic.secondary = c(
    "prp.symptomatic.secondary.msm",
    "prp.symptomatic.secondary.heterosexual_male",
    "prp.symptomatic.secondary.female"
  ),
  hiv.testing = c(
    "hiv.testing.or",
    "hiv.testing.slope.or"
  ),
  
  sti.screening.by.stage1=c(
    
    "sti.screening.multiplier.ps",    # b.PS.SCREENING
    "sti.screening.multiplier.el",
    "sti.screening.multiplier.ll"
  ),
  
  sti.screening.by.stage2=c(
    "sti.screening.multiplier.tertiary",
    "sti.screening.multiplier.cns"
  ),    
  screening.by.time1 = c(
    "syphilis.screening.multiplier.1980",
    "syphilis.screening.multiplier.1990"
  ),
  screening.by.time2 = c(
    "syphilis.screening.multiplier.2000",
    "syphilis.screening.multiplier.2010",
    "syphilis.screening.multiplier.2020"
  ),
  
  contact.tracing=c(
    "prop.index.cases.reached.for.contact.tracing"  # b.CONTACT.TRACING
  )
)


SHIELD.TESTING.SAMPLING.BLOCKS2=list(
  contact.tracing=c(
    "prop.index.cases.reached.for.contact.tracing")
)

# SUMMARIZE ---- #these will be registered in the specification 
SHIELD.FULL.PARAMETERS.PRIOR = distributions::join.distributions(
  POPULATION.PARAMETERS.PRIOR,
  AGING.PARAMETERS.PRIOR,
  TRANSMISSION.PARAMETERS.PRIOR,
  TESTING.PARAMETERS.PRIOR
)

SHIELD.FULL.PARAMETERS.SAMPLING.BLOCKS=c(
  SHIELD.POPULATION.SAMPLING.BLOCKS,
  SHIELD.AGING.SAMPLING.BLOCKS,
  SHIELD.TRANSMISSION.SAMPLING.BLOCKS,
  SHIELD.TESTING.SAMPLING.BLOCKS
)