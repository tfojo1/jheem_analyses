### Model EHE Intervention Effects on Racial Disparity in HIV Incidence

#source('../jheem2/R/tests/ENGINE_test.R')
#source("../jheem_analyses/applications/EHE/ehe_specification.R")

#Interventions are scaled up linearly from January 1st of START.YEAR to January 1st of IMPLEMENTED.BY.YEAR
START.YEAR = 2025
IMPLEMENTED.BY.YEAR = 2030

#Specify interventions on testing, viral suppression, and PrEP
testing.increase = create.intervention.effect('general.population.testing', #testing rate
                                              start.time = START.YEAR,
                                              effect.values = 'testing.multiplier',
                                              scale = 'rate',
                                              apply.effects.as = 'multiplier', #times what testing rate would have been on 1/1/2030
                                              times = IMPLEMENTED.BY.YEAR,
                                              allow.values.less.than.otherwise = F, #do not allow testing rate to decrease vs. NC
                                              allow.values.greater.than.otherwise = T) #allow testing rate to increase vs. NC

suppression.increase = create.intervention.effect('suppression.of.diagnosed', #percent suppressed
                                              start.time = START.YEAR,
                                              effect.values = 'unsuppressed.multiplier',
                                              scale = 'proportion.staying', #1-p
                                              apply.effects.as = 'multiplier', #times what % unsuppressed would have been on 1/1/2030
                                              times = IMPLEMENTED.BY.YEAR,
                                              allow.values.less.than.otherwise = F, #do not allow % suppressed to decrease vs. NC
                                              allow.values.greater.than.otherwise = T) #allow % suppressed to increase vs. NC

prep.increase = create.intervention.effect('oral.prep.uptake', #% initiated PrEP
                                                  start.time = START.YEAR,
                                                  effect.values = 'uninitiated.multiplier',
                                                  scale = 'proportion.staying', #1-p
                                                  apply.effects.as = 'multiplier', #times what % unsuppressed would have been on 1/1/2030
                                                  times = IMPLEMENTED.BY.YEAR,
                                                  allow.values.less.than.otherwise = F, #allow % suppressed to decrease vs. NC?
                                                  allow.values.greater.than.otherwise = T) #allow % suppressed to increase vs. NC?

#Create joint intervention and specify target population
base.intervention = create.intervention(WHOLE.POPULATION,
                                        testing.increase,
                                        suppression.increase,
                                        prep.increase)

#Specify criteria for checking whether the joint intervention met the EHE targets in 2030
testing.criterion = create.intervention.criterion(outcome = 'proportion.general.population.tested',
                                                  target.value = 0.951,
                                                  min.acceptable.value = 0.95,
                                                  max.acceptable.value = 0.96,
                                                  dimension.values = list(year=2030))
suppression.criterion = create.intervention.criterion(outcome = 'suppression',
                                                  target.value = 0.951,
                                                  min.acceptable.value = 0.95,
                                                  max.acceptable.value = 0.96,
                                                  dimension.values = list(year=2030))
prep.criterion = create.intervention.criterion(outcome = 'prep.uptake.proportion',
                                                  target.value = 0.51,
                                                  min.acceptable.value = 0.5,
                                                  max.acceptable.value = 0.55,
                                                  dimension.values = list(year=2030))

#Solve for parameter values that meet the criteria
full.intervention = create.criteria.based.intervention(base.intervention = base.intervention,
                                                       parameters.to.vary = c("testing.multiplier",
                                                                            "unsuppressed.multiplier",
                                                                            "uninitiated.multiplier"),
                                                       completion.criteria = list(testing.criterion,
                                                                                  suppression.criterion,
                                                                                  prep.criterion), 
                                                       parameter.scales = c(testing.multiplier = 'ratio',
                                                                            unsuppressed.multiplier = 'proportion.staying',
                                                                            uninitiated.multiplier = 'proportion.staying'),
                                                      #initial.parameter.values = function(sim){
                                                      #    2 / sim$get('testing', year='2026')},
                                                       initial.parameter.values = c(testing.multiplier = 5,
                                                                                   unsuppressed.multiplier = 0.1,
                                                                                   uninitiated.multiplier = 0.75),
                                                       #to limit computational time
                                                      max.iterations = 50,
                                                      max.failure.rate = 0.05)

test.intervention = create.intervention(WHOLE.POPULATION,
testing.increase,
suppression.increase,
prep.increase, parameter.distribution = join.distributions(
  testing.multiplier=Uniform.Distribution(4,6),
  unsuppressed.multiplier=Uniform.Distribution(0.09,0.11),
  uninitiated.multiplier=Uniform.Distribution(0.7,0.8)
),code="testdisp")
