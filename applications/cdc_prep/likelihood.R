
cdc.tests.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "hiv.tests", 
                                                                         outcome.for.sim = "cdc.funded.tests",
                                                                         dimensions = character(), #total
                                                                         levels.of.stratification = 0,
                                                                         from.year = 2010,
                                                                         to.year = 2019,
                                                                         observation.correlation.form = 'compound.symmetry',
                                                                         correlation.different.years = 0,
                                                                         error.variance.term = .015,
                                                                         error.variance.type = c('cv')
)

cdc.nonhealthcare.tests.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "non.healthcare.hiv.tests", 
                                                                         outcome.for.sim = "cdc.funded.tests.nonhealthcare", 
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



cdc.prep.referred.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "number.referred", 
                                                                                   outcome.for.sim = "cumulative.cdc.prep.referrals",
                                                                                   dimensions = character(), #total
                                                                                   levels.of.stratification = 0,
                                                                                   from.year = 2020,
                                                                                   to.year = 2025,
                                                                                   observation.correlation.form = 'compound.symmetry',
                                                                                   correlation.different.years = 0.5,
                                                                                   error.variance.term = 0.147, #how mis-measured is the target? 
                                                                                   error.variance.type = c('sd')
) 



cdc.prep.eligible.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "proportion.eligible", 
                                                                                   outcome.for.sim = "cumulative.fraction.eligible",
                                                                                   dimensions = character(), #total
                                                                                   levels.of.stratification = 0,
                                                                                   from.year = 2020,
                                                                                   to.year = 2022,
                                                                                   observation.correlation.form = 'compound.symmetry',
                                                                                   correlation.different.years = 0.5,
                                                                                   error.variance.term = 0.0845,
                                                                                   error.variance.type = c('sd')
)


cdc.joint.likelihood.instructions = join.likelihood.instructions(cdc.test.positivity.likelihood.instructions,cdc.nonhealthcaretests.likelihood.instructions,cdc.tests.likelihood.instructions,cdc.prep.referred.likelihood.instructions,cdc.prep.eligible.likelihood.instructions)



