
source("test/engine_test.R")
source("applications/cdc_testing/cdc_testing_specification.R")

transmuter = create.jheem.transmuter(simulation.set = sim,to.version = "cdct")

sim2 = transmuter$transmute(1)

#CDC Tests Likelihood Instructions 

cdc.tests.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "hiv.tests", 
                                                                         outcome.for.sim = "cdc.funded.tests",
                                                                         dimensions = character(), #total
                                                                         levels.of.stratification = 0,
                                                                         from.year = 2010,
                                                                         to.year = 2019,
                                                                         observation.correlation.form = 'compound.symmetry',
                                                                         correlation.different.years = 0.5,
                                                                         error.variance.term = .05,
                                                                         error.variance.type = c('cv')
                                                                         )

cdc.test.positivity.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "cdc.hiv.test.positivity", 
                                                                         outcome.for.sim = "total.cdc.hiv.test.positivity",
                                                                         dimensions = character(), #total
                                                                         levels.of.stratification = 0,
                                                                         from.year = 2010,
                                                                         to.year = 2019,
                                                                         observation.correlation.form = 'compound.symmetry',
                                                                         correlation.different.years = 0.5,
                                                                         error.variance.term = .001,
                                                                         error.variance.type = c('sd')
)


cdc.joint.likelihood.instructions = join.likelihood.instructions(cdc.test.positivity.likelihood.instructions,cdc.tests.likelihood.instructions)



