

#source("../jheem_analyses/applications/EHE/ehe_specification.R")
source('../jheem2/R/tests/ENGINE_test.R')

START.YEAR = 2025
IMPLEMENTED.BY.YEAR = 2030
testing.increase = create.intervention.effect('testing', 
                                              start.time = START.YEAR,
                                              effect.values = 'testing.multiplier',
                                              scale = 'rate',
                                              apply.effects.as = 'multiplier',
                                              times = IMPLEMENTED.BY.YEAR,
                                              allow.values.less.than.otherwise = F,
                                              allow.values.greater.than.otherwise = T)

base.intervention = create.intervention(WHOLE.POPULATION,
                                        testing.increase)


testing.criterion = create.intervention.criterion(outcome = 'testing',
                                                  target.value = 2,
                                                  min.acceptable.value = 1.9,
                                                  max.acceptable.value = 2.1,
                                                  dimension.values = list(year=2030))

full.intervention = create.criteria.based.intervention(base.intervention = base.intervention,
                                                       completion.criteria = list(testing.criterion), 
                                                       parameter.scales = c(
                                                          testing.multiplier = 'ratio'
                                                       ),
                                                       initial.parameter.values = function(sim){
                                                          2 / sim$get('testing', year='2026')
                                                       },
                                                       iterations.per.chunk = c(10,20,50),
                                                       max.chunks.before.giving.up = 3)
