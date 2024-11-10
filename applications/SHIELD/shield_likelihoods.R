# LIKELIHOODS INCLUDED: 


#-- POPULATION----
# Basic likelihood: where we have data at the location level desired
# sometimes we dont have the calibration data for the location of interest. 
# so for example we need to calibrate prop aware in Baltimiore to data from MD and building 
# some uncertainty to account for similarities between those locations
population.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "population", 
                                       outcome.for.sim = "population",
                                       dimensions = c("age","sex","race"),
                                       levels.of.stratification = c(0,1,2), # 0 = totals, 1 = 1-way stratification (e.g., age), 2 = 2-way stratification (e.g., age race)
                                       from.year = 2010, # the year calibration starts (population size and demographics are fix to 2007)
                                       #measurement error: if census data is off in one year, how much is it off the next year or different strata?
                                       correlation.different.years = 0.5, # this is the default
                                       correlation.different.strata = 0.1, # this is the default
                                       # correlation.different.sources = 0, # default from one source
                                       correlation.same.source.different.details = 0.3, # default: 
                                       
                                       # assumes correlation between all combos of years is the same
                                       observation.correlation.form = 'autoregressive.1', 
                                       
                                       # should always be specified; describes how precise the estimates are; 
                                       # e.g., estimates can be off by 3% each year
                                       error.variance.term = 0.015, 
                                       #error.variance.term = pop.year.cvs,  
                                       error.variance.type = 'cv'
                                       
                                       # downweight because large population size; 
                                       # can get more specific with create.likelihood.weights 
                                       #(e.g., different weight for age X)
                                       # weights = list(create.likelihood.weights(
                                       #   total.weight = 0.5,
                                       #   dimension.values = list(year = as.character(2007:2014)))), 
                                       
                                       # if there are more data points for certain years, this will normalize
                                       # e.g., if there are a few years with only the totals 
                                       # before the stratification are available
                                       # equalize.weight.by.year = F Default is TRUE
  )

#-- GENERAL MORTALITY  ----
# everyone in the population
total.mortality.likelihood.instructions =
  create.basic.likelihood.instructions(outcome.for.data = "deaths", #fix type
                                       outcome.for.sim = "total.mortality",
                                       dimensions = character(), #census only reports total deaths by location and year (those two are implicit)
                                       levels.of.stratification = c(0),
                                       from.year = 2007, # data available from c('2001-2010','2011-2020') # we are 
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.015, # in absence of data I am assuming the population level
                                       error.variance.type = 'cv'
                                       # weights = (18*TOTAL.WEIGHT), # see prev_new_aware_weighting.R
                                       # equalize.weight.by.year = T
  )
#-- BIRTHS  ----
births.likelihood.instructions =
  create.basic.likelihood.instructions(outcome.for.data = "births", #fix type
                                       outcome.for.sim = "births.from",
                                       dimensions = c("age","race"),
                                       levels.of.stratification = c(0,1,2), # 0 = totals, 1 = 1-way stratification (e.g., age), 2 = 2-way stratification (e.g
                                       from.year = 2007,  #data available from 2007-2023
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.015, # in absence of data I am assuming the population level
                                       error.variance.type = 'cv'
                                       # weights = (18*TOTAL.WEIGHT), # see prev_new_aware_weighting.R
                                       # equalize.weight.by.year = T
  )

#-- FETILITY RATE  ----
fertility.likelihood.instructions =
  create.basic.likelihood.instructions(outcome.for.data = "fertility.rate", #fix type
                                       outcome.for.sim = "fertility.rate",
                                       dimensions = c("age","race"),
                                       levels.of.stratification = c(0,1,2), # 0 = totals, 1 = 1-way stratification (e.g., age), 2 = 2-way stratification (e.g
                                       from.year = 2007,  #data available from 2007-2023
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.015, # in absence of data I am assuming the population level
                                       error.variance.type = 'cv'
  )


# if we work on proportions that it'll be different 



#-- FULL LIKELIHOODS --# ----
likelihood.instructions.demographics =  join.likelihood.instructions(
  population.likelihood.instructions ,
  total.mortality.likelihood.instructions,
  fertility.likelihood.instructions
  
)
#manual setup: 
# lik=population.likelihood.instructions$instantiate.likelihood('shield',"C.12580")
# lik=total.mortality.likelihood.instructions$instantiate.likelihood('shield',"C.12580")
# lik=fertility.likelihood.instructions$instantiate.likelihood('shield',"C.12580")
dimnames(SURVEILLANCE.MANAGER$data$fertility.rate$estimate$cdc.wonder.natality$cdc.fertility$year__location__age__race__ethnicity)
