


cdc.tests.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "hiv.tests", 
                                                                         outcome.for.sim = "O",
                                                                         dimensions = character(), #total
                                                                         levels.of.stratification = 0,
                                                                         from.year = 2010,
                                                                         to.year = 2019,
                                                                         observation.correlation.form = 'compound.symmetry',
                                                                         correlation.different.years = 0,
                                                                         error.variance.term = .005,
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
                                                                                   error.variance.term = .00005,
                                                                                   error.variance.type = c('sd')
)



cdc.prep.referred.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "number.eligible", 
                                                                                   outcome.for.sim = "cumulative.cdc.prep.eligible",
                                                                                   dimensions = character(), #total
                                                                                   levels.of.stratification = 0,
                                                                                   from.year = 2020,
                                                                                   to.year = 2025,
                                                                                   observation.correlation.form = 'compound.symmetry',
                                                                                   correlation.different.years = 0.5,
                                                                                   error.variance.term = .0009, 
                                                                                   error.variance.type = c('cv')
) 



cdc.prep.eligible.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "proportion.referred", 
                                                                                   outcome.for.sim = "cdc.fraction.prep.referred.of.eligible",
                                                                                   dimensions = character(), #total
                                                                                   levels.of.stratification = 0,
                                                                                   from.year = 2020,
                                                                                   to.year = 2022,
                                                                                   observation.correlation.form = 'compound.symmetry',
                                                                                   correlation.different.years = 0.5,
                                                                                   error.variance.term = 0.0845,
                                                                                   error.variance.type = c('sd')
)
 
cdc.prep.ratio.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "5.year.ratio", #target name 
                                                                                 outcome.for.sim = "log.ratio.cdc.funded.hiv.tests.2028.2023", #saved outcome
                                                                                 dimensions = character(), #total
                                                                                 levels.of.stratification = 0,
                                                                                 from.year = 2020,
                                                                                 to.year = 2022,
                                                                                 observation.correlation.form = 'compound.symmetry',
                                                                                 correlation.different.years = 0, #no correlation needed only one value per state 
                                                                                 error.variance.term = 0.44,
                                                                                 error.variance.type = c('sd')
)



cdc.prep.joint.likelihood.instructions = join.likelihood.instructions(cdc.test.positivity.likelihood.instructions,cdc.nonhealthcare.tests.likelihood.instructions,cdc.tests.likelihood.instructions,cdc.prep.referred.likelihood.instructions,cdc.prep.eligible.likelihood.instructions,cdc.prep.ratio.likelihood.instructions)



