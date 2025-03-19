# In the census manager: 
# anywhere that we have a county level data, we should have a national level data with the same stratification and ontology names 
# anywhere that we have a MSA level data, we should have a national level data with the same stratification and ontology names
# we can have more national level estimates if we can pull them directly 
# in the surveillance manager, we only need to have MSA level and national level data

# we should bound agegroups at 85+


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
#** POPULATION SIZES ** ----
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

#** DEATHS **  ----
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

#** FETILITY RATE **  ----
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

#** MIGRATION **  ----
#*#data is available for 6 overlapping 5-year period (2011-2015, 2012-2016, ....)
#*#one way stratifiation is only for 2011-2015
immigration.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "immigration", 
                                       outcome.for.sim = "immigration",
                                       dimensions = c('age','race','sex'), 
                                       levels.of.stratification = c(0,1,2),
                                       from.year = 2011, 
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.13, # using MOEs from data - see migration_MOE_summary ???
                                       error.variance.type = 'cv',
                                       weights = 1,
                                       equalize.weight.by.year = T
  )

emigration.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "emigration", 
                                       outcome.for.sim = "emigration",
                                       dimensions = c("age","race"), 
                                       levels.of.stratification = c(0,1,2),
                                       from.year = 2011, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.13, # using MOEs from data - see migration_MOE_summary
                                       error.variance.type = 'cv',
                                       weights = 1,
                                       equalize.weight.by.year = T
  )

#---- PRENATAL CARE COVERAGE -----: 
# we have 4 Categories representing a multinomial likelihood . 
# for now we are modeling 4 independant likelihoods. This may overpenalize deviations from a single bin because we are not accounting for the correlation between the 4 categories.)
# Error estimate: @zoe: what proportion of prenatals were unknown at the national level? we can use that to inform this error here 
prenatal.first.trimester.likelihood.instructions =
  create.basic.likelihood.instructions(outcome.for.data = "prp.prenatal.care.first.trimester", 
                                       outcome.for.sim = "prenatal.care.initiation.first.trimester",
                                       dimensions = c("age","race"),
                                       levels.of.stratification = c(0,1,2),
                                       from.year = 2010,
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.05, #5% error relative to the estimated value 
                                       error.variance.type = 'cv'
  )
prenatal.second.trimester.likelihood.instructions =
  create.basic.likelihood.instructions(outcome.for.data = "prp.prenatal.care.second.trimester", 
                                       outcome.for.sim = "prenatal.care.initiation.second.trimester",
                                       dimensions = c("age","race"),
                                       levels.of.stratification = c(0,1,2),
                                       from.year = 2010,
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.05,  
                                       error.variance.type = 'cv'
  )
prenatal.third.trimester.likelihood.instructions =
  create.basic.likelihood.instructions(outcome.for.data = "prp.prenatal.care.third.trimester", 
                                       outcome.for.sim = "prenatal.care.initiation.third.trimester",
                                       dimensions = c("age","race"),
                                       levels.of.stratification = c(0,1,2),
                                       from.year = 2010,
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.05,  
                                       error.variance.type = 'cv'
  )
no.prenatal.likelihood.instructions =
  create.basic.likelihood.instructions(outcome.for.data = "prp.no.prenatal.care", 
                                       outcome.for.sim = "no.prenatal.care",
                                       dimensions = c("age","race"),
                                       levels.of.stratification = c(0,1,2),
                                       from.year = 2010,
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.05,  
                                       error.variance.type = 'cv'
  )
#** SYPHILIS DIAGNOSIS ** ----
# we have modeled the misclassification of EL/LL diagnosis in the model and here we only fit to reported (biased) data
##---- PS ----
ps.diagnosis.likelihood.instructions =
  create.basic.likelihood.instructions(outcome.for.data = "ps.syphilis",  
                                       outcome.for.sim = "diagnosis.primary.secondary",  
                                       dimensions = c("age","race","sex"),
                                       levels.of.stratification = c(0,1,2), 
                                       from.year = 2000,
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.05, 
                                       error.variance.type = 'cv'
  )
##---- EARLY ----
#MISCLASSIFICATION ERROR 
early.diagnosis.likelihood.instructions =
  create.basic.likelihood.instructions(outcome.for.data = "early.syphilis", 
                                       outcome.for.sim = "diagnosis.el.misclassified",
                                       dimensions = c("age","race","sex"),
                                       levels.of.stratification = c(0,1,2,3),
                                       from.year = 2000,
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.05, 
                                       error.variance.type = 'cv'
  )
##---- Late/Unknown ----
late.diagnosis.likelihood.instructions =
  create.basic.likelihood.instructions(outcome.for.data = "unknown.duration.or.late.syphilis", 
                                       outcome.for.sim = "diagnosis.late.misclassified", #late latent misclassified + tertiary+cns
                                       dimensions = c("age","race","sex"),
                                       levels.of.stratification = c(0,1,2,3),
                                       from.year = 2000,
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.05, 
                                       error.variance.type = 'cv'
  )
##---- CNS ----
cns.diagnosis.likelihood.instructions =
  create.basic.likelihood.instructions(outcome.for.data = "neurosyphilis", 
                                       outcome.for.sim = "diagnosis.cns",
                                       dimensions = c("age","race","sex"),
                                       levels.of.stratification = c(0,1,2),
                                       from.year = 2000,
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.05, 
                                       error.variance.type = 'cv'
  )
##---- Congenital ----
congenital.diagnosis.likelihood.instructions =
  create.basic.likelihood.instructions(outcome.for.data = "congenital.syphilis", #fix type
                                       outcome.for.sim = "diagnosis.congenital",
                                       dimensions = c("age","race","sex"),
                                       levels.of.stratification = c(0,1,2),
                                       from.year = 2010,
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.05, 
                                       error.variance.type = 'cv'
  )


##---- Total ----
total.diagnosis.likelihood.instructions =
  create.basic.likelihood.instructions(outcome.for.data = "all.syphilis.cases",  
                                       outcome.for.sim = "diagnosis.total",
                                       dimensions = c("age","race","sex"),
                                       levels.of.stratification = c(0,1,2),
                                       from.year = 1941,
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.05, 
                                       error.variance.type = 'cv')
##---- Primary ----
# primary.diagnosis.likelihood.instructions =
#   create.basic.likelihood.instructions(outcome.for.data = "primary.syphilis",  
#                                        outcome.for.sim = "diagnosis.primary",
#                                        dimensions = c("age","race","sex"),
#                                        levels.of.stratification = c(0,1,2),
#                                        from.year = 1941,
#                                        observation.correlation.form = 'compound.symmetry',
#                                        error.variance.term = 0.05, 
#                                        error.variance.type = 'cv')
##---- Secondary ----
# secondary.diagnosis.likelihood.instructions =
#   create.basic.likelihood.instructions(outcome.for.data = "secondary.syphilis",  
#                                        outcome.for.sim = "secondary.total",
#                                        dimensions = c("age","race","sex"),
#                                        levels.of.stratification = c(0,1,2),
#                                        from.year = 1941,
#                                        observation.correlation.form = 'compound.symmetry',
#                                        error.variance.term = 0.05, 
#                                        error.variance.type = 'cv')



##** PRENATAL CARE COVERAGE ** ----#'@Todd: the function to capture incompleteness index
# prp.prenatal.care.first.trimester
prenatal.care.first.trimester.likelihood.instructions =
  create.basic.likelihood.instructions(outcome.for.data = "prp.prenatal.care.first.trimester",  
                                       outcome.for.sim = "prp.prenatal.care.first.trimester",
                                       dimensions = c("age","race"),
                                       levels.of.stratification = c(0,1),
                                       from.year = 2010,
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.05, 
                                       error.variance.type = 'cv')
prenatal.care.second.trimester.likelihood.instructions =
  create.basic.likelihood.instructions(outcome.for.data = "prp.prenatal.care.second.trimester",  
                                       outcome.for.sim = "prp.prenatal.care.second.trimester",
                                       dimensions = c("age","race"),
                                       levels.of.stratification = c(0,1),
                                       from.year = 2010,
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.05,  
                                       error.variance.type = 'cv')
prenatal.care.third.trimester.likelihood.instructions =
  create.basic.likelihood.instructions(outcome.for.data = "prp.prenatal.care.third.trimester",  
                                       outcome.for.sim = "prp.prenatal.care.third.trimester",
                                       dimensions = c("age","race"),
                                       levels.of.stratification = c(0,1),
                                       from.year = 2010,
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.05, 
                                       error.variance.type = 'cv')
##** HIV TESTS ** ----
hiv.testing.likelihood.instructions =
  create.basic.likelihood.instructions(outcome.for.data = "proportion.tested.for.hiv", 
                                       outcome.for.sim = "hiv.testing",
                                       dimensions = c("age","race","sex"),
                                       levels.of.stratification = c(0,1,2),
                                       from.year = 2014,
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.05, 
                                       error.variance.type = 'cv'
                                       )

#-- FULL LIKELIHOODS --# ----
likelihood.instructions.demographics =  join.likelihood.instructions(
  population.likelihood.instructions ,
  deaths.likelihood.instructions,
  fertility.likelihood.instructions,
  
  immigration.likelihood.instructions,
  emmigration.likelihood.instructions,
  
  prenatal.first.trimester.likelihood.instructions,
  prenatal.second.trimester.likelihood.instructions,
  prenatal.third.trimester.likelihood.instructions,
  no.prenatal.likelihood.instructions,
  
  hiv.testing.likelihood.instructions,
  
  ps.diagnosis.likelihood.instructions,
  early.diagnosis.likelihood.instructions,
  late.diagnosis.likelihood.instructions,
  cns.diagnosis.likelihood.instructions,
  congenital.diagnosis.likelihood.instructions,
  total.diagnosis.likelihood.instructions,
)
#manual setup: 
# lik=population.likelihood.instructions$instantiate.likelihood('shield',"US")
# lik=deaths.likelihood.instructions$instantiate.likelihood('shield',"US")
# lik=fertility.likelihood.instructions$instantiate.likelihood('shield',"US")
# dimnames(SURVEILLANCE.MANAGER$data$fertility.rate$estimate$cdc.wonder.natality$cdc.fertility$year__location__age__race__ethnicity)



# estiamting errors for syphilis stages: # Zoe will be estimating this based on discrepency between CDC reported numbers and those reported from local health departments
# ps.syphilis: @Zoe: we have 0,1,2 way interactions for this, but for early.syphilis, we also have the 3 way interaction
# for unknown.duration.or.late.syphilis we only have year__location__age
# Zoe: what about primary.syphilis and secondary.syphilis estimates? 


# lik=ps.diagnosis.likelihood.instructions$instantiate.likelihood('shield',"C.12580")
#dimnames(SURVEILLANCE.MANAGER$data$ps.syphilis$estimate$cdc.sti$cdc.sti$year__location__sex)
#this is reported for male/female: no msm 

# can we ssume rates of diagnosis among het-male and female are similar and the rest of new diagnosis among male are msm specific 
# @Zoe: can you please add a mapping for this? male= msm + het_male
