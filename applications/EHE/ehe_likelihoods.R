
# need to register instructions first? 

population.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "population",
                                     outcome.for.sim = "population",
                                     dimensions = c("age","sex","race"),
                                     levels.of.stratification = c(0,1), # 0 = totals, 0-way stratification; 1-way stratification
                                     from.year = as.integer(2007),
                                     correlation.different.years = 0.5, # this is the default
                                     correlation.different.strata = 0.1, # this is the default
                                     correlation.different.sources = 0.3, # default
                                     correlation.same.source.different.details = 0.3, # default
                                     
                                     # assumes correlation between all combos of years is the same
                                     observation.correlation.form = 'compound.symmetry', 
                                     
                                     # should always be specified; describes how precise the estimates are; 
                                     # e.g., estimates can be off by 3% each year - LOOK THIS UP
                                     measurement.error.coefficient.of.variance = 0.03,
                                     
                                     # downweight because large population size; 
                                     # can get more specific with create.likelihood.weights (e.g., different weight for age X)
                                     weights = list(1/64),
                                     
                                     # if there are more datapoints for certain years, this will normalize
                                     # e.g., if there are a few years with only the totals before the stratifications are available
                                     equalize.weight.by.year = T 
                                     )

# this gives instructions; major function is instantiate.likelihood
# population.likelihood.instructions$instantiate.likelihood()