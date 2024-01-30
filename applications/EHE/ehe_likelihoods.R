
# sim$outcomes
# SURVEILLANCE.MANAGER$outcomes
# SURVEILLANCE.MANAGER$get.ontologies.for.outcome("diagnoses")

immigration.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "adult.immigration", # fix this 
                                                                           outcome.for.sim = "immigration",
                                                                           dimensions = c("race"), # eventually will include age
                                                                           levels.of.stratification = c(0,1),
                                                                           from.year = 2011, # may break
                                                                           
                                                                           observation.correlation.form = 'compound.symmetry', 
                                                                           measurement.error.coefficient.of.variance = 0.05, # look up how far off migration data may be
                                                                           weights = 1,
                                                                           equalize.weight.by.year = T 
)

emigration.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "adult.emigration", # fix this 
                                                                          outcome.for.sim = "emigration",
                                                                          dimensions = c("race"), # eventually will include age
                                                                          levels.of.stratification = c(0,1),
                                                                          from.year = 2011, # may break
                                                                          
                                                                          observation.correlation.form = 'compound.symmetry', 
                                                                          measurement.error.coefficient.of.variance = 0.05, # look up how far off migration data may be
                                                                          weights = 1,
                                                                          equalize.weight.by.year = T 
)

# imm.lik = immigration.likelihood.instructions$instantiate.likelihood(version = 'ehe', location = 'C.12580')

# this gives instructions; major function is instantiate.likelihood
# population.likelihood.instructions$instantiate.likelihood()
population.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                                                          outcome.for.sim = "population",
                                                                          dimensions = c("age","sex","race"),
                                                                          levels.of.stratification = c(0,1,2), # 0 = totals, 1 = 1-way stratification
                                                                          from.year = as.integer(2007),
                                                                          correlation.different.years = 0.5, # this is the default
                                                                          correlation.different.strata = 0.1, # this is the default
                                                                          correlation.different.sources = 0.3, # default
                                                                          correlation.same.source.different.details = 0.3, # default
                                                                          
                                                                          # assumes correlation between all combos of years is the same
                                                                          observation.correlation.form = 'compound.symmetry', 
                                                                          
                                                                          # should always be specified; describes how precise the estimates are; 
                                                                          # e.g., estimates can be off by 3% each year 
                                                                          measurement.error.coefficient.of.variance = 0.03,
                                                                          
                                                                          # downweight because large population size; 
                                                                          # can get more specific with create.likelihood.weights 
                                                                          #(e.g., different weight for age X)
                                                                          weights = 1, 
                                                                          
                                                                          # if there are more datapoints for certain years, this will normalize
                                                                          # e.g., if there are a few years with only the totals 
                                                                          # before the stratifications are available
                                                                          equalize.weight.by.year = T 
)

joint.pop.migration.likelihood.instructions = join.likelihood.instructions(population.likelihood.instructions,
                                                                           immigration.likelihood.instructions,
                                                                           emigration.likelihood.instructions
                                                                           )

risk.new.diagnoses.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                                                             outcome.for.sim = "new",
                                                                             dimensions = c("risk"),
                                                                             levels.of.stratification = c(0,1), 
                                                                             from.year = as.integer(2008), 
                                                                             
                                                                             observation.correlation.form = 'compound.symmetry', 
                                                                             measurement.error.coefficient.of.variance = 0.03,
                                                                             
                                                                             weights = list(1), # upweight?
                                                                             equalize.weight.by.year = T 
)

risk.prevalence.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                                                          outcome.for.sim = "diagnosed.prevalence",
                                                                          dimensions = c("risk"),
                                                                          levels.of.stratification = c(0,1), 
                                                                          from.year = as.integer(2008), 
                                                                          
                                                                          observation.correlation.form = 'compound.symmetry', 
                                                                          measurement.error.coefficient.of.variance = 0.03,
                                                                          
                                                                          weights = list(1), # upweight?
                                                                          equalize.weight.by.year = T 
)

new.diagnoses.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                                                             outcome.for.sim = "new",
                                                                             dimensions = c("age","sex","race","risk"),
                                                                             levels.of.stratification = c(0,1,2), 
                                                                             from.year = as.integer(2008), 
                                                                             
                                                                             observation.correlation.form = 'compound.symmetry', 
                                                                             measurement.error.coefficient.of.variance = 0.03,
                                                                             
                                                                             weights = list(1), # upweight?
                                                                             equalize.weight.by.year = T 
)

prevalence.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                                                          outcome.for.sim = "diagnosed.prevalence",
                                                                          dimensions = c("age","sex","race","risk"),
                                                                          levels.of.stratification = c(0,1,2), 
                                                                          from.year = as.integer(2008), 
                                                                          
                                                                          observation.correlation.form = 'compound.symmetry', 
                                                                          measurement.error.coefficient.of.variance = 0.03,
                                                                          
                                                                          weights = list(1), # upweight?
                                                                          equalize.weight.by.year = T 
)

mortality.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "hiv.deaths",
                                                                         outcome.for.sim = "hiv.mortality",
                                                                         dimensions = c("sex"),
                                                                         levels.of.stratification = c(0,1), 
                                                                         from.year = as.integer(2008), 
                                                                         
                                                                         observation.correlation.form = 'compound.symmetry', 
                                                                         measurement.error.coefficient.of.variance = 0.03,
                                                                         
                                                                         weights = list(1), 
                                                                         equalize.weight.by.year = T 
)


one.way.transmission.likelihood.instructions = join.likelihood.instructions(population.likelihood.instructions,
                                                            immigration.likelihood.instructions,
                                                            emigration.likelihood.instructions,
                                                            risk.new.diagnoses.likelihood.instructions,
                                                            risk.prevalence.likelihood.instructions
)



## OLD POPULATION LIKELIHOODS
if(1==2){
  population.two.way.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                                                                    outcome.for.sim = "population",
                                                                                    dimensions = c("age","sex","race"),
                                                                                    levels.of.stratification = c(0,1,2), # 0 = totals, 1 = 1-way stratification
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
                                                                                    # can get more specific with create.likelihood.weights 
                                                                                    #(e.g., different weight for age X)
                                                                                    weights = 1/32, ### UPDATED 12/13
                                                                                    
                                                                                    # if there are more datapoints for certain years, this will normalize
                                                                                    # e.g., if there are a few years with only the totals 
                                                                                    # before the stratifications are available
                                                                                    equalize.weight.by.year = T 
  )
  
  
  population.one.way.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                                                                    outcome.for.sim = "population",
                                                                                    dimensions = c("age","sex","race"),
                                                                                    levels.of.stratification = c(0,1), # 0 = totals, 1 = 1-way stratification
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
                                                                                    # can get more specific with create.likelihood.weights 
                                                                                    #(e.g., different weight for age X)
                                                                                    weights = list(1/64),
                                                                                    
                                                                                    # if there are more datapoints for certain years, this will normalize
                                                                                    # e.g., if there are a few years with only the totals 
                                                                                    # before the stratifications are available
                                                                                    equalize.weight.by.year = T 
  )
  
  population.total.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                                                                  outcome.for.sim = "population",
                                                                                  dimensions = c("age","sex","race"),
                                                                                  levels.of.stratification = c(0), # 0 = totals, 1 = 1-way stratification
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
                                                                                  # can get more specific with create.likelihood.weights 
                                                                                  #(e.g., different weight for age X)
                                                                                  weights = list(1/64),
                                                                                  
                                                                                  # if there are more datapoints for certain years, this will normalize
                                                                                  # e.g., if there are a few years with only the totals 
                                                                                  # before the stratifications are available
                                                                                  equalize.weight.by.year = T 
  )
  
}

