#EHE Intervention Effects on Racial Disparities in HIV Incidence
#Code to specify interventions of interest.

#source('../jheem2/R/tests/ENGINE_test.R')
source("../jheem_analyses/applications/EHE/ehe_specification.R")

#Interventions are scaled up linearly from January 1st of START.YEAR to January 1st of IMPLEMENTED.BY.YEAR
START.YEAR = 2025
IMPLEMENTED.BY.YEAR = 2030

#No intervention
noint=get.null.intervention()

#Individual interventions on testing, viral suppression, and PrEP
testing.increase = create.intervention.effect('general.population.testing', #testing rate
                                              start.time = START.YEAR,
                                              effect.values = 'testing.multiplier',
                                              scale = 'rate',
                                              apply.effects.as = 'multiplier', #times what testing rate would have been on 1/1/2030
                                              times = IMPLEMENTED.BY.YEAR,
                                              allow.values.less.than.otherwise = F, #do not allow testing rate to decrease vs. NC
                                              allow.values.greater.than.otherwise = T #allow testing rate to increase vs. NC
                                              )

suppression.increase = create.intervention.effect('suppression.of.diagnosed', #percent suppressed
                                              start.time = START.YEAR,
                                              effect.values = 'unsuppressed.multiplier',
                                              scale = 'proportion.staying', #1-p
                                              apply.effects.as = 'multiplier', #times what % unsuppressed would have been on 1/1/2030
                                              times = IMPLEMENTED.BY.YEAR,
                                              allow.values.less.than.otherwise = F, #do not allow % suppressed to decrease vs. NC
                                              allow.values.greater.than.otherwise = T #allow % suppressed to increase vs. NC
                                              )

prep.increase = create.intervention.effect('oral.prep.uptake', #% initiated PrEP
                                              start.time = START.YEAR,
                                              effect.values = 'uninitiated.multiplier',
                                              scale = 'proportion.staying', #1-p
                                              apply.effects.as = 'multiplier', #times what % uninitiated would have been on 1/1/2030
                                              times = IMPLEMENTED.BY.YEAR,
                                              allow.values.less.than.otherwise = F, #allow % initiated to decrease vs. NC?
                                              allow.values.greater.than.otherwise = T #allow % initiated to increase vs. NC?
                                              )

#Create joint intervention and specify target population
base.intervention = create.intervention(WHOLE.POPULATION,
                                        testing.increase,
                                        suppression.increase,
                                        prep.increase)

testing.criterion = create.monotonic.criterion(parameter.name = 'testing.multiplier',
                                               outcome = 'awareness', #percent aware of status #(used to be 'testing')
                                               parameter.scale = 'ratio',
                                               parameter.initial.value = 2,
                                               target.value = .95, #.95
                                               min.acceptable.value = .94,
                                               max.acceptable.value = .96,
                                               dimension.values=list(year='2030'),
                                               min.acceptable.parameter.value = 1,
                                               max.acceptable.parameter.value = 10)
suppression.criterion = create.monotonic.criterion(parameter.name = 'unsuppressed.multiplier',
                                                   outcome = 'suppression', #percent suppressed
                                                   parameter.scale = 'complementary.proportion',
                                                   parameter.initial.value = .5,
                                                   target.value = .9025, #95% of 95%
                                                   min.acceptable.value = .9,
                                                   max.acceptable.value = .905,
                                                   dimension.values=list(year='2030'),
                                                   min.acceptable.parameter.value = 0.05,
                                                   max.acceptable.parameter.value = 1)
prep.criterion = create.monotonic.criterion(parameter.name = 'uninitiated.multiplier',
                                            outcome = 'prep.uptake.proportion', #percent prescribed PrEP or enrolled in a PrEP program
                                            parameter.scale = 'complementary.proportion',
                                            parameter.initial.value = .5,
                                            target.value = .5, #.52
                                            min.acceptable.value = .49,
                                            max.acceptable.value = .51,
                                            dimension.values=list(year='2030'),
                                            min.acceptable.parameter.value = 0.05,
                                            max.acceptable.parameter.value = 1)


#Create full criteria-based intervention
full.int = create.monotonic.criteria.based.intervention (base.intervention = base.intervention,
                                                         completion.criteria = list(testing.criterion,
                                                                                    suppression.criterion,
                                                                                    prep.criterion),
                                                         max.iterations = 20,
                                                         n.iterations.after.satisfying.criteria = 5,
                                                         max.iterations.first.sim = 100,
                                                         n.iterations.after.satisfying.criteria.first.sim = 20,
                                                         max.failure.rate = 0,
                                                         code="fullint", 
                                                         name=NULL,
                                                         overwrite.existing.intervention=TRUE)

#Test full intervention
#x=full.int$run(simset, start.year=2025, end.year=2030, verbose=TRUE)