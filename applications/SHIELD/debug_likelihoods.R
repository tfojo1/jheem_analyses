# additional likelihoods to test the model
population.likelihood.instructions.2way.sex.race = 
    create.basic.likelihood.instructions(outcome.for.sim = "population",
                                         outcome.for.data = "population", 
                                         na.rm =T,
                                         dimensions = c("sex","race"),
                                         levels.of.stratification = c(2), # 0 = totals, 1 = 1-way stratification (e.g., age), 2 = 2-way stratification (e.g., age race)
                                         from.year = 2010, # the year calibration starts (population size and demographics are fix to 2007)
                                         #measurement error: if census data is off in one year, how much is it off the next year or different strata?
                                         correlation.different.years = 0.5, # this is the default
                                         correlation.different.strata = 0.1, # this is the default
                                         correlation.same.source.different.details = 0.3, # default: 
                                         observation.correlation.form = 'autoregressive.1', 
                                         error.variance.term = 0.015, 
                                         error.variance.type = 'cv',
                                         weights = w,
                                         equalize.weight.by.year = F #if we dont have as many data points in one year it'll be upweighted
    )
population.likelihood.instructions.2way.sex.age= 
    create.basic.likelihood.instructions(outcome.for.sim = "population",
                                         outcome.for.data = "population", 
                                         na.rm =T,
                                         dimensions = c("sex","age"),
                                         levels.of.stratification = c(2), # 0 = totals, 1 = 1-way stratification (e.g., age), 2 = 2-way stratification (e.g., age race)
                                         from.year = 2010, # the year calibration starts (population size and demographics are fix to 2007)
                                         #measurement error: if census data is off in one year, how much is it off the next year or different strata?
                                         correlation.different.years = 0.5, # this is the default
                                         correlation.different.strata = 0.1, # this is the default
                                         correlation.same.source.different.details = 0.3, # default: 
                                         observation.correlation.form = 'autoregressive.1', 
                                         error.variance.term = 0.015, 
                                         error.variance.type = 'cv',
                                         weights = w,
                                         equalize.weight.by.year = F #if we dont have as many data points in one year it'll be upweighted
    )
population.likelihood.instructions.2way.age.race = 
    create.basic.likelihood.instructions(outcome.for.sim = "population",
                                         outcome.for.data = "population", 
                                         na.rm =T,
                                         dimensions = c("age","race"),
                                         levels.of.stratification = c(2), # 0 = totals, 1 = 1-way stratification (e.g., age), 2 = 2-way stratification (e.g., age race)
                                         from.year = 2010, # the year calibration starts (population size and demographics are fix to 2007)
                                         #measurement error: if census data is off in one year, how much is it off the next year or different strata?
                                         correlation.different.years = 0.5, # this is the default
                                         correlation.different.strata = 0.1, # this is the default
                                         correlation.same.source.different.details = 0.3, # default: 
                                         observation.correlation.form = 'autoregressive.1', 
                                         error.variance.term = 0.015, 
                                         error.variance.type = 'cv',
                                         weights = w,
                                         equalize.weight.by.year = F #if we dont have as many data points in one year it'll be upweighted
    )

population.likelihood.instructions.1way.age = 
    create.basic.likelihood.instructions(outcome.for.sim = "population",
                                         outcome.for.data = "population", 
                                         na.rm =T,
                                         dimensions = c("age"),
                                         levels.of.stratification = c(1), # 0 = totals, 1 = 1-way stratification (e.g., age), 2 = 2-way stratification (e.g., age race)
                                         from.year = 2010, # the year calibration starts (population size and demographics are fix to 2007)
                                         #measurement error: if census data is off in one year, how much is it off the next year or different strata?
                                         correlation.different.years = 0.5, # this is the default
                                         correlation.different.strata = 0.1, # this is the default
                                         correlation.same.source.different.details = 0.3, # default: 
                                         observation.correlation.form = 'autoregressive.1', 
                                         error.variance.term = 0.015, 
                                         error.variance.type = 'cv',
                                         weights = w,
                                         equalize.weight.by.year = F #if we dont have as many data points in one year it'll be upweighted
    )
