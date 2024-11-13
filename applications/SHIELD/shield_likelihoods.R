# LIKELIHOODS INCLUDED: 

#the census runs population count every 10 years, in 2010, and 2020.
# the values reported between these years are extrapolated based on the previous census data.
# so we have more faith in those years and less int he ones between decades
w1=lapply(2010:2019, function(year){
  total.weight = 0.95^(year-2010)
  create.likelihood.weights(total.weight,dimension.values = list(year=year))
  })
w2=lapply(2020:2023, function(year){
  total.weight = 0.95^(year-2020)
  create.likelihood.weights(total.weight,dimension.values = list(year=year))
})
w=c(w1,w2)
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
                                       error.variance.type = 'cv',
                                       
                                       # downweight because large population size; 
                                       # can get more specific with create.likelihood.weights 
                                       #(e.g., different weight for age X)
                                       weights = w,
                                       equalize.weight.by.year = F #if we dont have as many data points in one year it'll be upweighted
                                       #in years that we have more data points we will downweight them
                                       
                                       # if there are more data points for certain years, this will normalize
                                       # e.g., if there are a few years with only the totals 
                                       # before the stratification are available
                                       # equalize.weight.by.year = F Default is TRUE
  )

#-- DEATHS  ----
# CalibTarget: deaths: 2001-2020 by agegroup,sex, race, ethnicty for the US model 
# dimnames(SURVEILLANCE.MANAGER$data$deaths$estimate$cdc_wonder$census.cdc.wonder.births.deaths$year__location__age__race__ethnicity__sex)

deaths.likelihood.instructions =
  create.basic.likelihood.instructions(outcome.for.data = "deaths", #fix type
                                       outcome.for.sim = "deaths",
                                       dimensions = c("age","sex","race"),
                                       levels.of.stratification = c(0,1,2), 
                                       from.year = 2010, 
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.015, # in absence of data I am assuming the population level
                                       error.variance.type = 'cv'
                                       # weights = (18*TOTAL.WEIGHT), # see prev_new_aware_weighting.R
                                       # equalize.weight.by.year = T
  )

#-- FETILITY RATE  ----
# CalibTarget: Fertility.rate: 2007-2023 agegroup race ethnicty 
# dimnames(SURVEILLANCE.MANAGER$data$fertility.rate$estimate$cdc.wonder.natality$cdc.fertility$year__location__age__race__ethnicity)

fertility.likelihood.instructions =
  create.basic.likelihood.instructions(outcome.for.data = "fertility.rate", #fix type
                                       outcome.for.sim = "fertility.rate",
                                       dimensions = c("age","race"),
                                       levels.of.stratification = c(0,1,2), # 0 = totals, 1 = 1-way stratification (e.g., age), 2 = 2-way stratification (e.g
                                       from.year = 2010,  #data available from 2007-2023
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.015, # in absence of data I am assuming the population level
                                       error.variance.type = 'cv'
  )


# if we work on proportions that it'll be different 



#-- FULL LIKELIHOODS --# ----
likelihood.instructions.demographics =  join.likelihood.instructions(
  population.likelihood.instructions ,
  deaths.likelihood.instructions,
  fertility.likelihood.instructions
  
)
#manual setup: 
# lik=population.likelihood.instructions$instantiate.likelihood('shield',"US")
# lik=deaths.likelihood.instructions$instantiate.likelihood('shield',"US")
# lik=fertility.likelihood.instructions$instantiate.likelihood('shield',"55139")
# lik=fertility.likelihood.instructions$instantiate.likelihood('shield',"US")
# dimnames(SURVEILLANCE.MANAGER$data$fertility.rate$estimate$cdc.wonder.natality$cdc.fertility$year__location__age__race__ethnicity)
