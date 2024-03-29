
# LIKELIHOODS INCLUDED: 
# population, immigration, emigration, new diagnoses, prevalence, hiv mortality, general mortality, 
# AIDS diagnoses, AIDS deaths, suppression

# TO DO: 
# proportion tested - working out bugs
# hiv.test.positivity

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
                                                                           dimensions = c("age","race"), 
                                                                           levels.of.stratification = c(0,1),
                                                                           from.year = 2011, 
                                                                           observation.correlation.form = 'compound.symmetry', 
                                                                           measurement.error.coefficient.of.variance = 0.13, # using MOEs from data - see migration_MOE_summary
                                                                           weights = 1,
                                                                           equalize.weight.by.year = T 
)

#-- EMIGRATION  --#
emigration.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "adult.emigration", 
                                                                          outcome.for.sim = "emigration",
                                                                          dimensions = c("age","race"), 
                                                                          levels.of.stratification = c(0,1),
                                                                          from.year = 2011, 
                                                                          observation.correlation.form = 'compound.symmetry', 
                                                                          measurement.error.coefficient.of.variance = 0.13, # using MOEs from data - see migration_MOE_summary
                                                                          weights = 1,
                                                                          equalize.weight.by.year = T 
)


#-- JOIN THE POPULATION-RELATED LIKELIHOODS  --#
joint.pop.migration.likelihood.instructions = join.likelihood.instructions(population.likelihood.instructions,
                                                                           immigration.likelihood.instructions,
                                                                           emigration.likelihood.instructions
                                                                           )
#-- NEW DIAGNOSES  --#
race.risk.sex.two.way.new.diagnoses.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                       outcome.for.sim = "new",
                                       dimensions = c("race","risk","sex"),
                                       levels.of.stratification = c(0,1,2), 
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
race.risk.sex.two.way.prevalence.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                       outcome.for.sim = "diagnosed.prevalence",
                                       dimensions = c("race","risk","sex"),
                                       levels.of.stratification = c(0,1,2), 
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


#-- JOIN THE TRANSMISSION-RELATED AND POPULATION LIKELIHOODS  --#
two.way.transmission.pop.likelihood.instructions = 
  join.likelihood.instructions(race.risk.sex.two.way.new.diagnoses.likelihood.instructions,
                               race.risk.sex.two.way.prevalence.likelihood.instructions,
                               population.likelihood.instructions # no aids
                               
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

#-- BIAS ESTIMATES FOR NESTED PROPORTIONS  --#
suppression.bias.estimates = get.cached.object.for.version(name = "suppression.bias.estimates", 
                                                           version = 'ehe')

proportion.tested.bias.estimates = get.cached.object.for.version(name = "proportion.tested.bias.estimates", 
                                                                 version = 'ehe')  

hiv.test.positivity.bias.estimates = get.cached.object.for.version(name = "hiv.test.positivity.bias.estimates", 
                                                                   version = 'ehe')  

awareness.bias.estimates = get.cached.object.for.version(name = "awareness.bias.estimates", 
                                                         version = 'ehe')  

cocaine.bias.estimates = get.cached.object.for.version(name = "cocaine.bias.estimates", 
                                                       version = 'ehe')  

heroin.bias.estimates = get.cached.object.for.version(name = "heroin.bias.estimates", 
                                                      version = 'ehe')  


EHE.PARTITIONING.FUNCTION = function(arr, version='ehe', location)
{
  if ("risk" %in% names(dim(arr)) &&
      all(array.access(arr, risk='active_IDU')==array.access(arr, risk='IDU_in_remission')))
  {
    risk.partition.dimnames = list(risk = c('active_IDU', 'IDU_in_remission'))
    risk.partition.arr = array(c(0.25, 0.75), dim=sapply(risk.partition.dimnames, length), risk.partition.dimnames)
    risk.modified = array.access(arr, risk.partition.dimnames)
    risk.modified = risk.modified * expand.array(risk.partition.arr, dimnames(risk.modified))
    array.access(arr, dimnames(risk.modified)) = risk.modified
  }
  if ("sex" %in% names(dim(arr)) &&
      all(array.access(arr, sex='msm')==array.access(arr, sex='heterosexual_male')))
  {
    #sex.partition.dimnames = list(sex = c('heterosexual_male', 'msm'))
    #sex.partition.arr = array(c(0.5, 0.5), dim=sapply(sex.partition.dimnames, length), sex.partition.dimnames)
    
    specification.metadata = get.specification.metadata(version=version, location=location)
    proportion.msm = get.best.guess.msm.proportions(location,
                                                    specification.metadata = specification.metadata,
                                                    keep.age = any(names(dim(arr))=='age'),
                                                    keep.race = any(names(dim(arr))=='race'))
    # sex.partition.arr = get.best.guess.msm.proportions.by.race(location,
    #                                                            specification.metadata = specification.metadata,
    #                                                            years = DEFAULT.POPULATION.YEARS,
    #                                                            min.age = specification.metadata$age.lower.bounds[1],
    #                                                            return.proportions = T)
    
    sex.partition.arr = c(as.numeric(proportion.msm), 1-as.numeric(proportion.msm))
    sex.partition.dimnames = c(dimnames(proportion.msm), list(sex=c('msm','heterosexual_male')))
    dim(sex.partition.arr) = sapply(sex.partition.dimnames, length)
    dimnames(sex.partition.arr) = sex.partition.dimnames
    
    sex.modified = array.access(arr, sex.partition.dimnames)
    sex.modified = sex.modified * expand.array(sex.partition.arr, dimnames(sex.modified))
    array.access(arr, dimnames(sex.modified)) = sex.modified
  }
  arr
  
}


#-- SUPPRESSION  --#
suppression.likelihood.instructions = 
  create.nested.proportion.likelihood.instructions(outcome.for.data = "suppression",
                                                   outcome.for.sim = "suppression",
                                                   denominator.outcome.for.data = 'diagnosed.prevalence',
                                                   denominator.outcome.for.sim = 'diagnosed.prevalence',
                                                   
                                                   # want to be able to specify max # for each location type;
                                                   # have to decide how to order (probably by denominator)
                                                   location.types = c('COUNTY','STATE','CBSA'), 
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   # limit.to.n.location
                                                   
                                                   # dimensions = c("age","sex","race","risk"),
                                                   dimensions = c("sex"),
                                                   levels.of.stratification = c(0,1), 
                                                   from.year = as.integer(2008), 
                                                   
                                                   p.bias.inside.location = suppression.bias.estimates$in.mean, 
                                                   p.bias.outside.location = suppression.bias.estimates$out.mean,
                                                   p.bias.sd.inside.location = suppression.bias.estimates$in.sd,
                                                   p.bias.sd.outside.location = suppression.bias.estimates$out.sd,
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry', 
                                                   measurement.error.sd = 0.03,
                                                   
                                                   partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                   
                                                   weights = list(1), 
                                                   equalize.weight.by.year = T 
  )

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

#-- AWARENESS --#
awareness.likelihood.instructions =
  create.nested.proportion.likelihood.instructions(outcome.for.data = "awareness",
                                                   outcome.for.sim = "awareness",
                                                   denominator.outcome.for.data = "total.prevalence",
                                                   outcome.for.n.multipliers = "diagnosed.prevalence",
                                                   
                                                   location.types = c('STATE','CBSA','COUNTY'),
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   
                                                   dimensions = character(), # would like to write NULL
                                                   levels.of.stratification = 0, # would like to have an auto of 0:length(d)
                                                   
                                                   
                                                   from.year = as.integer(2008),
                                                   
                                                   p.bias.inside.location = 0, # awareness.bias.estimates$in.mean is NA
                                                   p.bias.outside.location = awareness.bias.estimates$out.mean,
                                                   p.bias.sd.inside.location = awareness.bias.estimates$out.sd, # awareness.bias.estimates$in.sd is NA
                                                   p.bias.sd.outside.location = awareness.bias.estimates$out.sd,
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry',
                                                   measurement.error.sd = .016, # .018*90 - rough estimate from HIV Atlas (for now)
                                                   # @Andrew want two arguments here: 
                                                   # measurement.error.term = NULL,
                                                   # measurement.error.type = "data.cv", 
                                                   # options: cv, sd, data.cv (pull cv off of data), data.interval (take lower/upper bounds from data)
                                                   
                                                   partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                   
                                                   weights = list(1),
                                                   equalize.weight.by.year = T
  )

#-- HEROIN  --#
heroin.likelihood.instructions = 
  create.nested.proportion.likelihood.instructions(outcome.for.data = "heroin",
                                                   outcome.for.sim = "proportion.using.heroin",
                                                   denominator.outcome.for.data = 'adult.population',
                                                   denominator.outcome.for.sim = 'population',
                                                   
                                                   location.types = c('STATE','NSDUH'), 
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   
                                                   dimensions = c("age"),
                                                   levels.of.stratification = c(0,1), 
                                                   from.year = as.integer(2008), 
                                                   
                                                   p.bias.inside.location = 0, 
                                                   p.bias.outside.location = heroin.bias.estimates$out.mean,
                                                   p.bias.sd.inside.location = heroin.bias.estimates$out.sd,
                                                   p.bias.sd.outside.location = heroin.bias.estimates$out.sd,
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry', 
                                                   measurement.error.sd = 0.54*0.005, # for now, double the NSDUH calcs and multiply by .005 from MD data
                                                   # measurement.error = 0.27, # NSDUH calcs 
                                                   # measurement.error.type = "cv",
                                                   
                                                   partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                   
                                                   weights = list(1), 
                                                   equalize.weight.by.year = T 
  )

#-- COCAINE  --#
cocaine.likelihood.instructions = 
  create.nested.proportion.likelihood.instructions(outcome.for.data = "cocaine",
                                                   outcome.for.sim = "proportion.using.cocaine",
                                                   denominator.outcome.for.data = 'adult.population',
                                                   denominator.outcome.for.sim = 'population',
                                                   
                                                   location.types = c('STATE','NSDUH'), 
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   
                                                   dimensions = c("age"),
                                                   levels.of.stratification = c(0,1), 
                                                   from.year = as.integer(2008), 
                                                   
                                                   p.bias.inside.location = 0, 
                                                   p.bias.outside.location = cocaine.bias.estimates$out.mean,
                                                   p.bias.sd.inside.location = cocaine.bias.estimates$out.sd,
                                                   p.bias.sd.outside.location = cocaine.bias.estimates$out.sd,
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry', 
                                                   measurement.error.sd = 0.42*0.02, # for now, double the NSDUH calcs and multiply by .02 from MD data
                                                   # measurement.error.term = 0.21, # NSDUH calcs 
                                                   # measurement.error.type = "cv",
                                                   
                                                   partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                   
                                                   weights = list(1), 
                                                   equalize.weight.by.year = T 
  )



# LIKELIHOOD COMPONENT STILL NOT WORKING
if(1==2){
  #-- PROPORTION TESTED --#
  proportion.tested.likelihood.instructions =
    create.nested.proportion.likelihood.instructions(outcome.for.data = "proportion.tested",
                                                     outcome.for.sim = "proportion.general.population.tested",
                                                     denominator.outcome.for.data = "adult.population",
                                                     
                                                     location.types = c('STATE','CBSA'),
                                                     minimum.geographic.resolution.type = 'COUNTY',
                                                     
                                                     dimensions = c("age","sex","race","risk"),
                                                     levels.of.stratification = c(0,1),
                                                     from.year = as.integer(2008),
                                                     
                                                     p.bias.inside.location = 0, 
                                                     p.bias.outside.location = proportion.tested.bias.estimates$out.mean,
                                                     p.bias.sd.inside.location = proportion.tested.bias.estimates$out.sd,
                                                     p.bias.sd.outside.location = proportion.tested.bias.estimates$out.sd,
                                                     
                                                     within.location.p.error.correlation = 0.5,
                                                     within.location.n.error.correlation = 0.5,
                                                     
                                                     observation.correlation.form = 'compound.symmetry',
                                                     measurement.error.sd = 0.03,
                                                     
                                                     partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                     
                                                     weights = list(1),
                                                     equalize.weight.by.year = T
    )
  
  prop.tested.lik = proportion.tested.likelihood.instructions$instantiate.likelihood('ehe', 'C.12580')
  
  #-- HIV TEST POSITIVITY --#
  hiv.test.positivity.likelihood.instructions =
    create.nested.proportion.likelihood.instructions(outcome.for.data = "hiv.test.positivity",
                                                     outcome.for.sim = "hiv.test.positivity",
                                                     denominator.outcome.for.data = "hiv.tests",
                                                     #"total.hiv.tests",
                                                     
                                                     location.types = c('STATE','CBSA'),
                                                     minimum.geographic.resolution.type = 'COUNTY',
                                                     
                                                     dimensions = character(),
                                                     levels.of.stratification = c(0),
                                                     from.year = as.integer(2008),
                                                     
                                                     p.bias.inside.location = 0, 
                                                     p.bias.outside.location = hiv.test.positivity.bias.estimates$out.mean,
                                                     p.bias.sd.inside.location = hiv.test.positivity.bias.estimates$out.sd,
                                                     p.bias.sd.outside.location = hiv.test.positivity.bias.estimates$out.sd,
                                                     
                                                     within.location.p.error.correlation = 0.5,
                                                     within.location.n.error.correlation = 0.5,
                                                     
                                                     observation.correlation.form = 'compound.symmetry',
                                                     measurement.error.sd = 0.03,
                                                     
                                                     partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                     
                                                     weights = list(1),
                                                     equalize.weight.by.year = T
    )
  
  hiv.test.positivity.lik = hiv.test.positivity.likelihood.instructions$instantiate.likelihood('ehe', 'C.12580')
  hiv.test.positivity.lik$compute(sim)
  
  
  

  }




