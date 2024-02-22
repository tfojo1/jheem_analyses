
# LIKELIHOODS INCLUDED: 
# population, immigration, emigration, new diagnoses, prevalence, hiv mortality, general mortality, AIDS diagnoses

# TO DO: 
# fix year range issue: AIDS deaths
# fix aggregations to MSA level: prep uptake, prep indications
# nested proportions: suppression,  proportion tested, awareness, IDU (heroin/cocaine ratios)

#-- POPULATION  --#
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

#-- IMMIGRATION  --#
immigration.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "adult.immigration", 
                                                                           outcome.for.sim = "immigration",
                                                                           dimensions = c("race"), # eventually will include age
                                                                           levels.of.stratification = c(0,1),
                                                                           from.year = 2011, 
                                                                           observation.correlation.form = 'compound.symmetry', 
                                                                           measurement.error.coefficient.of.variance = 0.05, # look up how far off migration data may be
                                                                           weights = 1,
                                                                           equalize.weight.by.year = T 
)

#-- EMIGRATION  --#
emigration.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "adult.emigration", 
                                                                          outcome.for.sim = "emigration",
                                                                          dimensions = c("race"), # eventually will include age
                                                                          levels.of.stratification = c(0,1),
                                                                          from.year = 2011, 
                                                                          observation.correlation.form = 'compound.symmetry', 
                                                                          measurement.error.coefficient.of.variance = 0.05, # look up how far off migration data may be
                                                                          weights = 1,
                                                                          equalize.weight.by.year = T 
)


#-- JOIN THE POPULATION-RELATED LIKELIHOODS  --#
joint.pop.migration.likelihood.instructions = join.likelihood.instructions(population.likelihood.instructions,
                                                                           immigration.likelihood.instructions,
                                                                           emigration.likelihood.instructions
                                                                           )
#-- NEW DIAGNOSES  --#
race.risk.one.way.new.diagnoses.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                                                             outcome.for.sim = "new",
                                                                             dimensions = c("race","risk"),
                                                                             levels.of.stratification = c(0,1), 
                                                                             from.year = as.integer(2008), 
                                                                             observation.correlation.form = 'compound.symmetry', 
                                                                             measurement.error.coefficient.of.variance = 0.03,
                                                                             weights = list(1), 
                                                                             equalize.weight.by.year = T 
)
new.diagnoses.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                                                             outcome.for.sim = "new",
                                                                             dimensions = c("age","sex","race","risk"),
                                                                             levels.of.stratification = c(0,1,2), 
                                                                             from.year = as.integer(2008), 
                                                                             observation.correlation.form = 'compound.symmetry', 
                                                                             measurement.error.coefficient.of.variance = 0.03,
                                                                             weights = list(1), 
                                                                             equalize.weight.by.year = T 
)


#-- PREVALENCE  --#
race.risk.one.way.prevalence.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                                                                            outcome.for.sim = "diagnosed.prevalence",
                                                                                            dimensions = c("race","risk"),
                                                                                            levels.of.stratification = c(0,1), 
                                                                                            from.year = as.integer(2008), 
                                                                                            observation.correlation.form = 'compound.symmetry', 
                                                                                            measurement.error.coefficient.of.variance = 0.03,
                                                                                            weights = list(1),
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


#-- AIDS DIAGNOSES  --#
aids.diagnoses.likelihood.instructions = 
  create.basic.likelihood.instructions.with.included.multiplier(outcome.for.data = "aids.diagnoses",
                                                                outcome.for.sim = "aids.diagnoses", 
                                                                dimensions = c("race","risk"), # ADD BACK AGE AND SEX LATER
                                                                levels.of.stratification = c(0,1), 
                                                                from.year = as.integer(1980),
                                                                to.year = as.integer(2001),
                                                                observation.correlation.form = 'compound.symmetry', 
                                                                measurement.error.coefficient.of.variance = 0.05, # maybe higher - look up
                                                                weights = list(1), 
                                                                equalize.weight.by.year = T,
                                                                included.multiplier = 1.4,
                                                                included.multiplier.sd = 0.2, # rounding up from 10%, 0.14
                                                                included.multiplier.correlation = 0.5
  )

#-- JOIN THE TRANSMISSION-RELATED LIKELIHOODS  --#
one.way.transmission.and.aids.likelihood.instructions = 
  join.likelihood.instructions(race.risk.one.way.new.diagnoses.likelihood.instructions,
                               race.risk.one.way.prevalence.likelihood.instructions,
                               aids.diagnoses.likelihood.instructions
                               
  )

#-- HIV-MORTALITY  --#
# all-cause mortality among pwh
hiv.mortality.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "hiv.deaths",
                                                                             outcome.for.sim = "hiv.mortality", 
                                                                             dimensions = c("sex"),
                                                                             levels.of.stratification = c(0,1), 
                                                                             from.year = as.integer(2008), 
                                                                             observation.correlation.form = 'compound.symmetry', 
                                                                             measurement.error.coefficient.of.variance = 0.03,
                                                                             weights = list(1), 
                                                                             equalize.weight.by.year = T 
)

#-- GENERAL MORTALITY  --#
# everyone in the population, regardless of HIV 
general.mortality.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "deaths",
                                                                         outcome.for.sim = "total.mortality", 
                                                                         dimensions = character(),
                                                                         levels.of.stratification = c(0), 
                                                                         from.year = as.integer(2007), 
                                                                         observation.correlation.form = 'compound.symmetry', 
                                                                         measurement.error.coefficient.of.variance = 0.03, 
                                                                         weights = list(1), 
                                                                         equalize.weight.by.year = T 
)

#-- SUPPRESSION  --#
# Right now there's a data manager error that messes this up - check applied_test file for how to pick source 
# this is going to be too slow - have a separate file that runs and saves all the p.bias.estimates 
# and then load those when I actually do the likelihoods
# suppression.bias.estimates = get.p.bias.estimates(SURVEILLANCE.MANAGER,
#                                                  dimensions = c("age","race","sex","risk"),
#                                                  levels.of.stratification = c(0,1),
#                                                  outcome.for.p = "suppression",
#                                                  outcome.for.n = "diagnosed.prevalence",
#                                                  sub.location.type = "COUNTY",
#                                                  super.location.type = "STATE",
#                                                  main.location.type = "CBSA")
# proportion.tested.bias.estimates = get.p.bias.estimates(SURVEILLANCE.MANAGER,
#                                                         dimensions = c("age","race","sex","risk"),
#                                                         levels.of.stratification = c(0,1),
#                                                         outcome.for.p = "proportion.tested",
#                                                         outcome.for.n = "adult.population",
#                                                         sub.location.type = "COUNTY",
#                                                         super.location.type = "STATE",
#                                                         main.location.type = "CBSA") 

p.bias.inside = 0
p.bias.outside = 0
p.bias.sd.inside = 0.03
p.bias.sd.outside = 0.03

if(1==2){
  ehe.partitioning.function = function(arr, version='ehe')
  {
    # LATER THERE WILL A DEFAULT PARTITIONING FUNCTION THAT IS USED IF THE USER LEAVES THE ARGUMENT NULL IN THE INSTRUCTIONS
    # For now, partition the different dimensions separately. Later, could have it do only one partition, a custom combined one, if you have risk and sex together
    dimensions.to.partition = intersect(c('risk', 'sex'), names(dim(arr)))
    # if (identical(dimensions.to.partition, c('risk')))
    if ("risk" %in% names(dim(arr))) {
      risk.partition.dimnames = list(risk = c('active_IDU', 'IDU_in_remission'))
      risk.partition.arr = array(c(0.25, 0.75), dim=sapply(risk.partition.dimnames, length), risk.partition.dimnames)
      risk.modified = array.access(arr, risk.partition.dimnames)
      risk.modified = risk.modified * expand.array(risk.partition.arr, dimnames(risk.modified))
      array.access(arr, dimnames(risk.modified)) = risk.modified
    }
    if ("sex" %in% names(dim(arr))) {
      #sex.partition.dimnames = list(sex = c('heterosexual_male', 'msm'))
      #sex.partition.arr = array(c(0.5, 0.5), dim=sapply(sex.partition.dimnames, length), sex.partition.dimnames)
      
      specification.metadata = get.specification.metadata(version=version, location=location)
      sex.partition.arr = get.best.guess.msm.proportions.by.race(location,
                                                                 specification.metadata = specification.metadata,
                                                                 years = DEFAULT.POPULATION.YEARS,
                                                                 min.age = specification.metadata$age.lower.bounds[1],
                                                                 return.proportions = T)
      sex.partition.dimnames = dimnames(sex.partition.arr)
      browser()
      sex.modified = array.access(arr, sex.partition.dimnames)
      sex.modified = sex.modified * expand.array(sex.partition.arr, dimnames(sex.modified))
      array.access(arr, dimnames(sex.modified)) = sex.modified
    }
    arr
    
  }

  suppression.likelihood.instructions = 
    create.nested.proportion.likelihood.instructions(outcome.for.data = "suppression",
                                                     outcome.for.sim = "suppression",
                                                     denominator.outcome.for.data = 'diagnosed.prevalence',
                                                     denominator.outcome.for.sim = 'diagnosed.prevalence',
                                                     
                                                     location.types = c('COUNTY','STATE','CBSA'),
                                                     minimum.geographic.resolution.type = 'COUNTY',
                                                     
                                                     # dimensions = c("age","sex","race","risk"),
                                                     dimensions = c("sex"),
                                                     levels.of.stratification = c(0,1), 
                                                     from.year = as.integer(2008), 
                                                     
                                                     p.bias.inside.location = p.bias.inside, # suppression.bias.estimates$...
                                                     p.bias.outside.location = p.bias.outside,
                                                     p.bias.sd.inside.location = p.bias.sd.inside,
                                                     p.bias.sd.outside.location = p.bias.sd.outside,
                                                     
                                                     within.location.p.error.correlation = 0.5,
                                                     within.location.n.error.correlation = 0.5,
                                                     
                                                     observation.correlation.form = 'compound.symmetry', 
                                                     measurement.error.sd = 0.03,
                                                     
                                                     partitioning.function = ehe.partitioning.function, # there will be a default.partitioning.function
                                                     
                                                     weights = list(1), 
                                                     equalize.weight.by.year = T 
    )
  
    
}


# TO DO: 
# proportion tested, awareness, IDU (heroin/cocaine ratios)


#-- AIDS DEATHS  --#
# in the data, this is the cumulative estimate of aids.diagnoses.deceased.by.2001 from 1980-2001 
# e.g., 1995 aids.diagnoses.deceased.by.2001 gives everyone diagnosed in 1995 who is deceased by 2001 (NOT that they died in 1995)
# so cumulative total will be helpful to get totals by sex/race/risk
aids.deaths.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "aids.deaths", 
                                                                           outcome.for.sim = "aids.deaths", 
                                                                           dimensions = c("sex","race","risk"),
                                                                           levels.of.stratification = c(0,1), 
                                                                           from.year = as.integer(1981), 
                                                                           to.year = as.integer(2001),
                                                                           observation.correlation.form = 'compound.symmetry', 
                                                                           measurement.error.coefficient.of.variance = 0.05, # maybe higher - look up 
                                                                           weights = list(1), 
                                                                           equalize.weight.by.year = T 
)

#-- PREP UPTAKE  --#
prep.uptake.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "prep",
                                                                           outcome.for.sim = "prep.uptake", 
                                                                           dimensions = c("age","sex","race"),
                                                                           levels.of.stratification = c(0,1,2), 
                                                                           from.year = as.integer(2007),
                                                                           observation.correlation.form = 'compound.symmetry', 
                                                                           measurement.error.coefficient.of.variance = 0.03, 
                                                                           weights = list(1), 
                                                                           equalize.weight.by.year = T 
)

#-- PREP INDICATIONS  --#
# this is an absolute count, not a proportion 
prep.indications.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "prep.indications",
                                                                           outcome.for.sim = "prep.indications",
                                                                           dimensions = c("age","sex"),
                                                                           levels.of.stratification = c(0,1), 
                                                                           from.year = 2017, 
                                                                           to.year = 2018, # they carried forwar 2018 numbers 
                                                                           observation.correlation.form = 'compound.symmetry', 
                                                                           measurement.error.coefficient.of.variance = 0.5, # high uncertainty
                                                                           # ^ this means you can range from 0 to 2x the number of prep indications
                                                                           weights = list(1), 
                                                                           equalize.weight.by.year = T 
)


#-- PROPORTION TESTED --#
if(1==2){
  proportion.tested.likelihood.instructions =
    create.nested.proportion.likelihood.instructions(outcome.for.data = "proportion.tested",
                                                     outcome.for.sim = "proportion.tested",
                                                     denominator.outcome.for.data = "adult.population",
                                                     denominator.outcome.for.sim = "population",
                                                     
                                                     location.types = c('STATE','CBSA'),
                                                     minimum.geographic.resolution.type = 'COUNTY',
                                                     
                                                     dimensions = c("age","sex","race","risk"),
                                                     levels.of.stratification = c(0,1),
                                                     from.year = as.integer(2008),
                                                     
                                                     p.bias.inside.location = p.bias.inside, # proportion.tested.bias.estimates$...
                                                     p.bias.outside.location = p.bias.outside,
                                                     p.bias.sd.inside.location = p.bias.sd.inside,
                                                     p.bias.sd.outside.location = p.bias.sd.outside,
                                                     
                                                     within.location.p.error.correlation = 0.5,
                                                     within.location.n.error.correlation = 0.5,
                                                     
                                                     observation.correlation.form = 'compound.symmetry',
                                                     measurement.error.sd = 0.03,
                                                     
                                                     partitioning.function = ehe.partitioning.function, # there will be a default.partitioning.function
                                                     
                                                     weights = list(1),
                                                     equalize.weight.by.year = T
    )
  
}



