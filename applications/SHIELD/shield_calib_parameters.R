#my best guess for this parameter is different in different locations, so we formulate prior as a multiply of the best guess
# Defining the calibration parameters and prior distributions

#1- PARAMETER PRIORS:----

## POPULATION.PARAMETERS.PRIOR ----
POPULATION.PARAMETERS.PRIOR=join.distributions( 
  ## Fertility rates 
  # (6 agegroups, 3 race, 2 knots)-> max 36 params
  # we start with 6 age, and 3 race, parameters applied to both knots -> 9 total
  black.fertility.rate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)), 
  hispanic.fertility.rate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  other.fertility.rate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  
  age15.19.fertility.rate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age20.24.fertility.rate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age25.29.fertility.rate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age30.34.fertility.rate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age35.39.fertility.rate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age40.44.fertility.rate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  
  # Mortality rates by race:
  black.general.mortality.rate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  hispanic.general.mortality.rate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  other.general.mortality.rate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  # Mortality rates by sex:
  male.general.mortality.rate.multiplier = Lognormal.Distribution(meanlog = 0,sdlog = 0.5*log(2)), 
  female.general.mortality.rate.multiplier = Lognormal.Distribution(meanlog = 0,sdlog = 0.5*log(2))
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
  ## Transmission
  global.transmission.rate = Lognormal.Distribution(meanlog = log(3.5), sdlog = 0.5*log(2)), #directly used in specification (will need sth uch larger) 
  #
  transmission.rate.multiplier.msm0 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  transmission.rate.multiplier.msm1 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  transmission.rate.multiplier.msm2 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  ### by race:
  transmission.rate.multiplier.msm.black= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  transmission.rate.multiplier.msm.hispanic= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  
  ## Sexual Mixing by Age
  age.mixing.sd.mult = Lognormal.Distribution(0, 0.25*log(2)) #directly used in specification helper function
  #to control the standard deviation of the contact matrix by age
  
) 

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
  q=specification.metadata$age.upper.bounds
  aging.from=q[1: (length(q)-1)]-1
  

  ## Aging Rates ----
  #10 (ages) * 3 (races) * 3 sexes= 90 for 2 knots = 180
  for(i in c(1,2)){ #spline with 2 knots
    for(age.index in 1:length(aging.from)) {
      for(race in races){
        # for(s in sexes){
          agegroup=ages[age.index]
          # browser()
          paramName=paste0("age",aging.from[age.index],".",race,".aging.rate.multiplier.",i)
          set.element.functional.form.interaction.alphas(model.settings,
                                                         element.name = "rate.general.aging",
                                                         alpha.name = paste0("time",i),
                                                         value = parameters[paramName],
                                                         applies.to.dimension.values =list(age = agegroup, race = race))
        }}}                                      
  
  ## Fertility rates by race/age to time1/time2 knots----
  #when we have a function with knots, we use alpha.name = time (taking one at a time)
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "fertility.rate",
                                                 alpha.name = "time1",
                                                 values = parameters[paste0(races,".fertility.rate.multiplier")],
                                                 dimension = "race",
                                                 applies.to.dimension.values = races)
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "fertility.rate",
                                                 alpha.name = "time2",
                                                 values = parameters[paste0(races,".fertility.rate.multiplier")],
                                                 dimension = "race",
                                                 applies.to.dimension.values = races)
  
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "fertility.rate",
                                                 alpha.name = 'time1',
                                                 values = parameters[paste0("age",fertile.age.ranges,".fertility.rate.multiplier")],
                                                 dimension = "age",
                                                 applies.to.dimension.values = fertile.ages)
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "fertility.rate",
                                                 alpha.name = 'time2',
                                                 values = parameters[paste0("age",fertile.age.ranges,".fertility.rate.multiplier")],
                                                 dimension = "age",
                                                 applies.to.dimension.values = fertile.ages)
  
  
  ## Mortality rates by race----
  races=model.settings$specification.metadata$dim.names$race
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "rate.general.mortality",
                                                 alpha.name = 'value',
                                                 values = parameters[paste0(races,".general.mortality.rate.multiplier")],
                                                 dimension = "race",
                                                 applies.to.dimension.values = races)
  ## Mortality rates by sex----
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
  for(time in 0:2){
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "transmission.rate.msm",
                                                   alpha.name = paste0('time',time),
                                                   values = parameters[paste0("transmission.rate.multiplier.msm",time)],
                                                   dimension = 'all',
                                                   applies.to.dimension.values = 'all')
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "transmission.rate.msm",
                                                   alpha.name = paste0('time',time),
                                                   values = parameters[c("transmission.rate.multiplier.msm.black","transmission.rate.multiplier.msm.hispanic")],
                                                   dimension = "race.to", #recipient
                                                   applies.to.dimension.values = c("black","hispanic"))
  }
 
  
}



#3- SAMPLING BLOCKS: ----
# classic mcmc samples one param at a time, adaptive mcms samples multiple params
#could add more params here (1-5 per block)
## SHIELD.POPULATION.SAMPLING.BLOCKS ----
SHIELD.POPULATION.SAMPLING.BLOCKS = list(
  fertility.rates.race=c("black.fertility.rate.multiplier",
                         "hispanic.fertility.rate.multiplier",
                         "other.fertility.rate.multiplier"),
  
  fertility.rates.age=c("age15.19.fertility.rate.multiplier",
                        "age20.24.fertility.rate.multiplier",
                        "age25.29.fertility.rate.multiplier",
                        "age30.34.fertility.rate.multiplier",
                        "age35.39.fertility.rate.multiplier",
                        "age40.44.fertility.rate.multiplier"),
  
  mortality.rates.by.race=c("black.general.mortality.rate.multiplier",
                            "hispanic.general.mortality.rate.multiplier",
                            "other.general.mortality.rate.multiplier"),
  
  moratlity.rates.by.sex=c("male.general.mortality.rate.multiplier",
                           "female.general.mortality.rate.multiplier")
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
  global.transmission.rate=c("global.transmission.rate"),
  #
  msm.transmission = c(
    "transmission.rate.multiplier.msm0",
    "transmission.rate.multiplier.msm1",
    "transmission.rate.multiplier.msm2",
    "transmission.rate.multiplier.msm.black",
    "transmission.rate.multiplier.msm.hispanic"),
  #
  age.mixing.transmission=("age.mixing.sd.mult")
)

# SUMMARIZE ---- #these will be registered in the specification 
SHIELD.FULL.PARAMETERS.PRIOR = distributions::join.distributions(
  POPULATION.PARAMETERS.PRIOR,
  AGING.PARAMETERS.PRIOR,
  TRANSMISSION.PARAMETERS.PRIOR
)

SHIELD.FULL.PARAMETERS.SAMPLING.BLOCKS=c(
  SHIELD.POPULATION.SAMPLING.BLOCKS,
  SHIELD.AGING.SAMPLING.BLOCKS,
  SHIELD.TRANSMISSION.SAMPLING.BLOCKS
)