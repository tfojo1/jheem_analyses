#CDC Tests Likelihood Instructions 

cdc.tests.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "non.healthcare.hiv.tests", 
                                                                         outcome.for.sim = "prep", #outcome from model to be changed
                                                                         dimensions = character(), #total
                                                                         levels.of.stratification = 0,
                                                                         from.year = 2018,
                                                                         to.year = 2021,
                                                                         observation.correlation.form = 'compound.symmetry',
                                                                         correlation.different.years = 0,
                                                                         error.variance.term = .015,
                                                                         error.variance.type = c('sd')
)

cdc.test.positivity.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "cdc.hiv.test.positivity", 
                                                                                   outcome.for.sim = "total.cdc.hiv.test.positivity",
                                                                                   dimensions = character(), #total
                                                                                   levels.of.stratification = 0,
                                                                                   from.year = 2010,
                                                                                   to.year = 2019,
                                                                                   observation.correlation.form = 'compound.symmetry',
                                                                                   correlation.different.years = 0,
                                                                                   error.variance.term = .00015,
                                                                                   error.variance.type = c('sd')
)


cdc.joint.likelihood.instructions = join.likelihood.instructions(cdc.test.positivity.likelihood.instructions,cdc.tests.likelihood.instructions)



