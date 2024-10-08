# LIKELIHOODS INCLUDED: 
# population,
 # BIRTHS? By trata? 
# DEATHS? By strata?
 
#-- POPULATION----
# Basic likelihood: where we have data at the location level desired# sometimes we dont have the calibration data for the location of interest. so for example we need to calibrate prop aware in Baltimiore to data from MD and building some uncertainty to account for similarities between those locations
population.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "population", 
                                       outcome.for.sim = "population",
                                       dimensions = c("age","sex","race"),
                                       levels.of.stratification = c(0,1,2), # 0 = totals, 1 = 1-way stratification (e.g., age), 2 = 2-way stratification (e.g., age race)
                                       from.year = 2010, #the year calibration starts (popualtion size and demographics are fix to 2007)
                                       
                                       #measurement error: if census data is off in one year, how much is it off the next year or different strata?
                                       correlation.different.years = 0.5, # this is the default
                                       correlation.different.strata = 0.1, # this is the default
                                       correlation.different.sources = 0.3, # default
                                       correlation.same.source.different.details = 0.3, # default
                                       
                                       # assumes correlation between all combos of years is the same
                                       observation.correlation.form = 'autoregressive.1', 
                                       
                                       # should always be specified; describes how precise the estimates are; 
                                       # e.g., estimates can be off by 3% each year
                                       error.variance.term = 0.015, 
                                       #error.variance.term = pop.year.cvs,  
                                       error.variance.type = 'cv',
                                       
                                       # downweight because large population size; 
                                       # can get more specific with create.likelihood.weights 
                                       #(e.g., different weight for age X)
                                       # weights = list(create.likelihood.weights(
                                       #   total.weight = 0.5,
                                       #   dimension.values = list(year = as.character(2007:2014)))), 
                                       
                                       # if there are more datapoints for certain years, this will normalize
                                       # e.g., if there are a few years with only the totals 
                                       # before the stratifications are available
                                       equalize.weight.by.year = F
  )


#-- IMMIGRATION----
# immigration.likelihood.instructions = 
#   create.basic.likelihood.instructions(outcome.for.data = "immigration", 
#                                        outcome.for.sim = "immigration",
#                                        dimensions = c("age","race"), 
#                                        levels.of.stratification = c(0,1),
#                                        from.year = 2011, 
#                                        observation.correlation.form = 'compound.symmetry',
#                                        error.variance.term = 0.13, # using MOEs from data - see migration_MOE_summary
#                                        error.variance.type = 'cv',
#                                        weights = 1,
#                                        equalize.weight.by.year = T 
#   )
# 
# #-- EMIGRATION----
# emigration.likelihood.instructions = 
#   create.basic.likelihood.instructions(outcome.for.data = "emigration", 
#                                        outcome.for.sim = "emigration",
#                                        dimensions = c("age","race"), 
#                                        levels.of.stratification = c(0,1),
#                                        from.year = 2011, 
#                                        observation.correlation.form = 'compound.symmetry', 
#                                        error.variance.term = 0.13, # using MOEs from data - see migration_MOE_summary
#                                        error.variance.type = 'cv',
#                                        weights = 1,
#                                        equalize.weight.by.year = T 
#   )

#-- FULL LIKELIHOODS --# ----
FULL.likelihood.instructions =  join.likelihood.instructions(
  # POPULATION LIKELIHOODS
  population.likelihood.instructions 
  # immigration.likelihood.instructions, 
  # emigration.likelihood.instructions
)