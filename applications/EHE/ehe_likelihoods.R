# LIKELIHOODS INCLUDED: 
# population, immigration, emigration, new diagnoses, prevalence, hiv mortality, general mortality, 
# AIDS diagnoses, AIDS deaths, suppression, proportion.tested, hiv.test.positivity
# heroin, cocaine

TOTAL.WEIGHT = 1 # 0.5 universally downweighting to try to allow more mixing - NOT YET, TRYING WITH NEW CVs first 

#-- BIAS ESTIMATES FOR NESTED PROPORTIONS  ----
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

hiv.tests.per.population.bias.estimates = get.cached.object.for.version(name = "hiv.tests.per.population.bias.estimates", 
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
                                                    keep.race = any(names(dim(arr))=='race'),
                                                    ages = dimnames(arr)$age)
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

#-- POPULATION  ----

population.error.sd.fn = function(data, details=attr(data, 'details'))
{
    # Massage the data a big
    melted.data = reshape2::melt(data)
    years.since.preceding.census = melted.data$year %% 10
    years.from.nearest.census = pmin(years.since.preceding.census, -melted.data$year %% 10)
      
    # Set up error terms
    inherent.census.cv = 0.015
    
    stratified.dimension.candidates = c('age','race','sex')
    n.stratified.dimensions = length(intersect(names(dim(data)), stratified.dimension.candidates))
    
    if (n.stratified.dimensions<=1)
        max.post.censal.cv = 0.1561269 # from calculating_error_terms_for_ehe_likelihoods.R
    else
        max.post.censal.cv = 0.1939618
    
    max.post.censal.var = inherent.census.cv^2 + max.post.censal.cv^2
    
    post.censal.cv = exp(log(inherent.census.cv) + years.since.preceding.census * (0.5*log(max.post.censal.var) - log(inherent.census.cv)) / 9)
    WEIGHT.TO.INTERCENSAL.VS.POSTCENSAL = 4
    intercensal.cv = exp(log(inherent.census.cv) + years.from.nearest.census * (0.5*log(max.post.censal.var / WEIGHT.TO.INTERCENSAL.VS.POSTCENSAL) - log(inherent.census.cv)) / 9)  # assume variance is halved
    
    # this is a hack now - need to talk to Zoe about how she will formulate the details field
    # I just know that totals are intercensal at this time
    is.intercensal = grepl('intercensal', details, ignore.case = T) | n.stratified.dimensions==0
    cv = post.censal.cv
    cv[is.intercensal] = intercensal.cv
    
    data * cv
}

population.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                       outcome.for.sim = "population",
                                       dimensions = c("age","sex","race"),
                                       levels.of.stratification = c(0,1,2), # 0 = totals, 1 = 1-way stratification
                                       from.year = 2010,
                                       correlation.different.years = 0.5, # this is the default
                                       correlation.different.strata = 0.1, # this is the default
                                       correlation.different.sources = 0.3, # default
                                       correlation.same.source.different.details = 0.3, # default
                                       
                                       observation.correlation.form = 'autoregressive.1', 
                                       
                                       # should always be specified; describes how precise the estimates are; 
                                       # e.g., estimates can be off by 3% each year
#                                       error.variance.term = 0.015, 
 #                                      error.variance.type = 'cv',
                                       error.variance.term = population.error.sd.fn,
                                       error.variance.type = 'function.sd',
                                       
                                       weights = (1*TOTAL.WEIGHT),

                                       # if there are more datapoints for certain years, this will normalize
                                       # e.g., if there are a few years with only the totals 
                                       # before the stratifications are available
                                       equalize.weight.by.year = F
  )

#-- IMMIGRATION  ----
immigration.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.immigration", 
                                       outcome.for.sim = "immigration",
                                       dimensions = c("age","race"), 
                                       levels.of.stratification = c(0,1),
                                       from.year = 2011, 
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.13, # using MOEs from data - see migration_MOE_summary
                                       error.variance.type = 'cv',
                                       weights = (1*TOTAL.WEIGHT),
                                       equalize.weight.by.year = F
  )

#-- EMIGRATION  ----
emigration.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.emigration", 
                                       outcome.for.sim = "emigration",
                                       dimensions = c("age","race"), 
                                       levels.of.stratification = c(0,1),
                                       from.year = 2011, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.13, # using MOEs from data - see migration_MOE_summary
                                       error.variance.type = 'cv',
                                       weights = (1*TOTAL.WEIGHT),
                                       equalize.weight.by.year = F
  )


#-- NEW DIAGNOSES  ----
total.new.diagnoses.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                       outcome.for.sim = "new",
                                       levels.of.stratification = c(0), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.04621778, # from calculating_error_terms_for_ehe_likelihoods.R
                                       error.variance.type = 'cv',
                                       weights = (1*TOTAL.WEIGHT),
                                       equalize.weight.by.year = F
  )

new.diagnoses.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                       outcome.for.sim = "new",
                                       dimensions = c("age","sex","race","risk"),
                                       levels.of.stratification = c(0,1,2), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.04621778, # from calculating_error_terms_for_ehe_likelihoods.R
                                       error.variance.type = 'cv',
                                       weights = (1*TOTAL.WEIGHT), #list(0.3), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = F
  )

race.risk.new.diagnoses.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                       outcome.for.sim = "new",
                                       dimensions = c("race","risk"),
                                       levels.of.stratification = c(0,1,2), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.04621778, # from calculating_error_terms_for_ehe_likelihoods.R
                                       error.variance.type = 'cv',
                                       weights = (1*TOTAL.WEIGHT), #list(0.3), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = F
  )

#-- PREVALENCE  ----
total.prevalence.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                       outcome.for.sim = "diagnosed.prevalence",
                                       levels.of.stratification = c(0), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.04711922, # from calculating_error_terms_for_ehe_likelihoods.R
                                       error.variance.type = 'cv',
                                       weights = (1*TOTAL.WEIGHT),
                                       equalize.weight.by.year = F
  )

prevalence.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                       outcome.for.sim = "diagnosed.prevalence",
                                       dimensions = c("age","sex","race","risk"),
                                       levels.of.stratification = c(0,1,2), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.04711922, # from calculating_error_terms_for_ehe_likelihoods.R
                                       error.variance.type = 'cv',
                                       weights = (1*TOTAL.WEIGHT), #list(0.3), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = F
  )

race.risk.prevalence.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                       outcome.for.sim = "diagnosed.prevalence",
                                       dimensions = c("race","risk"),
                                       levels.of.stratification = c(0,1,2), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.04711922, # from calculating_error_terms_for_ehe_likelihoods.R
                                       error.variance.type = 'cv',
                                       weights = (1*TOTAL.WEIGHT), #list(0.3), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = F
  )

#-- AIDS DIAGNOSES  ----
non.age.aids.diagnoses.likelihood.instructions =
  create.basic.likelihood.instructions(outcome.for.data = "aids.diagnoses",
                                       outcome.for.sim = "aids.diagnoses",
                                       dimensions = c("sex","race","risk"),
                                       levels.of.stratification = c(0,1),
                                       from.year = 1985,
                                       to.year = 2001,
                                       correlation.different.years = 0.3,
                                       #observation.correlation.form = 'compound.symmetry',
                                       observation.correlation.form = 'autoregressive.1',
                                       error.variance.term = 0.04621778, # new diagnoses value from calculating_error_terms_for_ehe_likelihoods.R
                                       error.variance.type = 'cv',
                                       weights = (1*TOTAL.WEIGHT),
                                       equalize.weight.by.year = F
  )

#-- HIV-MORTALITY  ----
# all-cause mortality among pwh
hiv.mortality.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "hiv.deaths",
                                       outcome.for.sim = "hiv.mortality", 
                                       dimensions = c("sex"),
                                       levels.of.stratification = c(0,1), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.04711922, # using prevalence value from calculating_error_terms_for_ehe_likelihoods.R
                                       error.variance.type = 'cv',
                                       weights = (1*TOTAL.WEIGHT),
                                       equalize.weight.by.year = F
  )

#-- GENERAL MORTALITY  ----
# everyone in the population, regardless of HIV 
general.mortality.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "deaths",
                                       outcome.for.sim = "total.mortality", 
                                       dimensions = character(),
                                       levels.of.stratification = c(0), 
                                       from.year = 2007, 
                                       to.year = 2019,
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.03, # look into source and see if they have estimate 
                                       error.variance.type = 'cv',
                                       weights = (18*TOTAL.WEIGHT), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = F
  )

#-- SUPPRESSION  ----
suppression.likelihood.instructions = 
  create.nested.proportion.likelihood.instructions(outcome.for.data = "suppression",
                                                   outcome.for.sim = "suppression",
                                                   denominator.outcome.for.data = 'diagnosed.prevalence',
                                                   
                                                   # want to be able to specify max # for each location type;
                                                   # have to decide how to order (probably by denominator)
                                                   location.types = c('COUNTY','STATE','CBSA'), 
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   # limit.to.n.location
                                                   
                                                   dimensions = c("age","sex","race","risk"),
                                                   #dimensions = c("sex"),
                                                   levels.of.stratification = c(0,1), 
                                                   from.year = 2008, 
                                                   
                                                   maximum.locations.per.type = 2,
                                                   
                                                   p.bias.inside.location = suppression.bias.estimates$in.mean, 
                                                   p.bias.outside.location = suppression.bias.estimates$out.mean,
                                                   p.bias.sd.inside.location = suppression.bias.estimates$in.sd,
                                                   p.bias.sd.outside.location = suppression.bias.estimates$out.sd,
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry', 
                                                   p.error.variance.term = 0.04560282, # from calculating_error_terms_for_ehe_likelihoods.R
                                                   p.error.variance.type = 'sd',
                                                   
                                                   partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                   
                                                   weights = (1*TOTAL.WEIGHT),
                                                   equalize.weight.by.year = F
  )

#-- AIDS DEATHS  ----
# in the data, this is the cumulative estimate of aids.diagnoses.deceased.by.2001 from 1980-2001 
# e.g., 1995 aids.diagnoses.deceased.by.2001 gives everyone diagnosed in 1995 who is deceased by 2001 (NOT that they died in 1995)
# so cumulative total will be helpful to get totals by sex/race/risk
aids.deaths.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "aids.deaths", 
                                       outcome.for.sim = "aids.deaths", 
                                       dimensions = c("sex","race","risk"),
                                       levels.of.stratification = c(0,1), 
                                       from.year = 1981, 
                                       to.year = 2001,
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.2277531, # using aids diagnoses estimate from calculating_error_terms_for_ehe_likelihoods.R
                                       error.variance.type = 'cv',
                                       weights = (1*TOTAL.WEIGHT),
                                       equalize.weight.by.year = F
  )

#-- PREP UPTAKE  ----
prep.uptake.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "prep",
                                       outcome.for.sim = "prep.uptake", 
                                       dimensions = c("age","sex","race"),
                                       levels.of.stratification = c(0,1,2), 
                                       from.year = 2007,
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.01239159, # from calculating_error_terms_for_ehe_likelihoods.R
                                       error.variance.type = 'cv',
                                       weights = (0.3*TOTAL.WEIGHT), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = F
  )

#-- PREP INDICATIONS  ----
# this is an absolute count, not a proportion 
prep.indications.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "prep.indications",
                                       outcome.for.sim = "prep.indications",
                                       dimensions = c("age","sex"),
                                       levels.of.stratification = c(0,1), 
                                       from.year = 2017, 
                                       to.year = 2018, # they carried forward 2018 numbers 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.5, # high uncertainty,
                                       error.variance.type = 'cv',
                                       # ^ this means you can range from 0 to 2x the number of prep indications
                                       weights = (1*TOTAL.WEIGHT),
                                       equalize.weight.by.year = F 
  )

#-- AWARENESS ----
awareness.likelihood.instructions =
  create.nested.proportion.likelihood.instructions(outcome.for.data = "awareness",
                                                   outcome.for.sim = "awareness",
                                                   denominator.outcome.for.data = "total.prevalence",
                                                   outcome.for.n.multipliers = "diagnosed.prevalence",
                                                   
                                                   location.types = c('STATE','CBSA','COUNTY'),
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   
                                                   dimensions = character(), # would like to write NULL
                                                   levels.of.stratification = 0, # would like to have an auto of 0:length(d)
                                                   
                                                   from.year = 2008,
                                                   
                                                   p.bias.inside.location = awareness.bias.estimates$in.mean,
                                                   p.bias.outside.location = awareness.bias.estimates$out.mean,
                                                   p.bias.sd.inside.location = awareness.bias.estimates$in.sd, 
                                                   p.bias.sd.outside.location = awareness.bias.estimates$out.sd, 
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry',
                                                   p.error.variance.term = NULL, 
                                                   p.error.variance.type = 'data.cv', 
                                                   # the data estimate is a coefficient of variance 
                                                   
                                                   partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                   
                                                   weights = (18*TOTAL.WEIGHT), # see prev_new_aware_weighting.R 
                                                   equalize.weight.by.year = F
  )

#-- HEROIN  ----
heroin.likelihood.instructions = 
  create.nested.proportion.likelihood.instructions(outcome.for.data = "heroin",
                                                   outcome.for.sim = "proportion.using.heroin",
                                                   denominator.outcome.for.data = 'adult.population',
                                                   
                                                   location.types = c('STATE','NSDUH'), 
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   
                                                   dimensions = c("age"),
                                                   levels.of.stratification = c(0,1), 
                                                   from.year = 2008, 
                                                   
                                                   p.bias.inside.location = 0, 
                                                   p.bias.outside.location = heroin.bias.estimates$out.mean,
                                                   p.bias.sd.inside.location = heroin.bias.estimates$out.sd,
                                                   p.bias.sd.outside.location = heroin.bias.estimates$out.sd,
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry',
                                                   p.error.variance.term = 0.54, # NSDUH calcs; doubled value (0.27); see NSDUH IDU Data_updated.xlsx in input_managers
                                                   p.error.variance.type = "cv",
                                                   
                                                   partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                   
                                                   weights = (1*TOTAL.WEIGHT),
                                                   equalize.weight.by.year = F
  )

#-- COCAINE  ----
cocaine.likelihood.instructions = 
  create.nested.proportion.likelihood.instructions(outcome.for.data = "cocaine",
                                                   outcome.for.sim = "proportion.using.cocaine",
                                                   denominator.outcome.for.data = 'adult.population',
                                                   
                                                   location.types = c('STATE','NSDUH'), 
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   
                                                   dimensions = c("age"),
                                                   levels.of.stratification = c(0,1), 
                                                   from.year = 2008, 
                                                   
                                                   p.bias.inside.location = 0, 
                                                   p.bias.outside.location = cocaine.bias.estimates$out.mean,
                                                   p.bias.sd.inside.location = cocaine.bias.estimates$out.sd,
                                                   p.bias.sd.outside.location = cocaine.bias.estimates$out.sd,
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry', 
                                                   p.error.variance.term = 0.42, # NSDUH calcs doubled value (0.21); see NSDUH IDU Data_updated.xlsx in input_managers
                                                   p.error.variance.type = "cv",
                                                   
                                                   partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                   
                                                   weights = (1*TOTAL.WEIGHT),
                                                   equalize.weight.by.year = F
  )

#-- PROPORTION TESTED ----
proportion.tested.likelihood.instructions =
  create.nested.proportion.likelihood.instructions(outcome.for.data = "proportion.tested",
                                                   outcome.for.sim = "testing",
                                                   denominator.outcome.for.data = "adult.population",
                                                   
                                                   location.types = c('STATE','CBSA'),
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   
                                                   dimensions = c("age","sex","race","risk"),
                                                   levels.of.stratification = c(0,1),
                                                   from.year = 2010,
                                                   
                                                   p.bias.inside.location = 0, 
                                                   p.bias.outside.location = proportion.tested.bias.estimates$out.mean,
                                                   p.bias.sd.inside.location = proportion.tested.bias.estimates$out.sd,
                                                   p.bias.sd.outside.location = proportion.tested.bias.estimates$out.sd,
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry',
                                                   p.error.variance.term = NULL,
                                                   p.error.variance.type = 'data.variance', # sd or data.variance 
                                                   
                                                   partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                   
                                                   weights = (1*TOTAL.WEIGHT),
                                                   equalize.weight.by.year = F
  )

#-- HIV TEST POSITIVITY ----
hiv.test.positivity.likelihood.instructions =
  create.nested.proportion.likelihood.instructions.with.included.multiplier(outcome.for.data = "cdc.hiv.test.positivity",
                                                   outcome.for.sim = "cdc.hiv.test.positivity",
                                                   denominator.outcome.for.data = "hiv.tests",
                                                   outcome.for.n.multipliers = 'adult.population',
                                                   #"total.hiv.tests",
                                                   
                                                   location.types = c('STATE','CBSA'),
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   
                                                   dimensions = character(),
                                                   levels.of.stratification = c(0),
                                                   from.year = 2014,
                                                   to.year = 2020, # REMOVE WHEN ADULT POPULATION IS READY
                                                   redundant.location.threshold = 0,
                                                   
                                                   p.bias.inside.location = 0, 
                                                   p.bias.outside.location = hiv.test.positivity.bias.estimates$out.mean,
                                                   p.bias.sd.inside.location = hiv.test.positivity.bias.estimates$out.sd,
                                                   p.bias.sd.outside.location = hiv.test.positivity.bias.estimates$out.sd,
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry',
                                                   
                                                   included.multiplier = 2.810587, # from cdc_positivity.bias.R
                                                   included.multiplier.sd = sqrt(0.1391234), # from cdc_positivity.bias.R
                                                   included.multiplier.correlation = 0.5,
                                                   
                                                   p.error.variance.term = 0.5, # can be off by two fold; guessing this 
                                                   p.error.variance.type = 'cv',
                                                   
                                                   partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                   
                                                   weights = (18*TOTAL.WEIGHT), # see prev_new_aware_weighting.R 
                                                   equalize.weight.by.year = F
  )

#-- YEAR-ON-YEAR TESTS CHANGE ----

# nested version 
number.of.tests.year.on.year.change.nested.likelihood.instructions = 
  create.time.lagged.comparison.nested.proportion.likelihood.instructions(
    outcome.for.data = "hiv.tests.per.population",
    outcome.for.sim = "total.hiv.tests.per.population",
    
    denominator.outcome.for.data = "adult.population",
    
    location.types = c('STATE','CBSA'),
    minimum.geographic.resolution.type = 'COUNTY',
    
    levels.of.stratification = c(0),
    from.year = 2008,
    to.year = 2020, # REMOVE WHEN ADULT POPULATION IS READY
    
    p.bias.inside.location = 0, 
    p.bias.outside.location = hiv.tests.per.population.bias.estimates$out.mean,
    p.bias.sd.inside.location = hiv.tests.per.population.bias.estimates$out.sd,
    p.bias.sd.outside.location = hiv.tests.per.population.bias.estimates$out.sd,
    
    within.location.p.error.correlation = 0.5,
    within.location.n.error.correlation = 0.5,
    
    observation.correlation.form = 'compound.symmetry',
    p.error.variance.term = 0.03, # guessed this
    p.error.variance.type = 'cv',
    ratio.cv = 1.2,
    
    partitioning.function = EHE.PARTITIONING.FUNCTION, 
    
    weights = (18*TOTAL.WEIGHT), # see prev_new_aware_weighting.R 
    equalize.weight.by.year = F,
    use.lognormal.approximation = T
  )

# basic ratio likelihood (rolled back)
# number.of.tests.year.on.year.change.basic.likelihood.instructions = 
#   create.basic.ratio.likelihood.instructions(
#     outcome.for.data = "hiv.tests.per.population", 
#     outcome.for.sim = "total.hiv.tests.per.population",
#     
#     levels.of.stratification = c(0),
#     from.year = 2008,
#     
#     observation.correlation.form = 'compound.symmetry',
#     error.variance.term = 0.03, # guessed this
#     error.variance.type = 'cv',
#     
#     ratio.cv = 1.2,
#     
#     weights = (18*TOTAL.WEIGHT), # see prev_new_aware_weighting.R 
#     equalize.weight.by.year = F
#   )

# basic version - WHAT WE WANT TO EVENTUALLY USE
number.of.tests.year.on.year.change.basic.likelihood.instructions =
  create.time.lagged.comparison.likelihood.instructions(
    outcome.for.data = "hiv.tests.per.population",
    outcome.for.sim = "total.hiv.tests.per.population",
    
    levels.of.stratification = c(0),
    from.year = 2008,
    
    observation.correlation.form = 'compound.symmetry',
    error.variance.term = 0.03, # guessed this
    error.variance.type = 'cv',
    
    ratio.cv = 1.2,
    
    weights = (18*TOTAL.WEIGHT), # see prev_new_aware_weighting.R
    equalize.weight.by.year = F,
    use.lognormal.approximation = T
  )

# ifelse to try both versions 
number.of.tests.year.on.year.change.likelihood.instructions = 
  create.ifelse.likelihood.instructions(
    number.of.tests.year.on.year.change.basic.likelihood.instructions,
    number.of.tests.year.on.year.change.nested.likelihood.instructions
  )


#-- YEAR-ON-YEAR GONORRHEA CHANGE ----
gonorrhea.year.on.year.change.likelihood.instructions = 
  create.basic.ratio.likelihood.instructions(outcome.for.data = "gonorrhea.ratio", 
                                             outcome.for.sim = "sexual.transmission.rates", 
                                             # (2020 gon diagnoses / 2019 gon diagnoses) proportional to 
                                             # (2020 sexual transmisson/2019 sexual transmission)
                                             levels.of.stratification = c(0,1), 
                                             dimensions = c("sex","race","age"),
                                             from.year = 2018, 
                                             observation.correlation.form = 'compound.symmetry', 
                                             error.variance.term = 0.03, # if we can't find something better, use diagnoses estimate from above (0.07425679)
                                             error.variance.type = 'cv',
                                             correlation.different.years = 0.5,
                                             
                                             ratio.cv = 1.5,
                                             # ratio.correlation = , # NULL will enter default of 0
                                             
                                             weights = (1*TOTAL.WEIGHT),
                                             equalize.weight.by.year = F 
  )


# gonorrhea.year.on.year.change.likelihood.instructions = 
#   create.time.lagged.comparison.likelihood.instructions.with.included.multiplier(outcome.for.data = "gonorrhea.ratio", 
#                                                                                  outcome.for.sim = "sexual.transmission.rates", 
#                                                                                  # (2020 gon diagnoses / 2019 gon diagnoses) proportional to 
#                                                                                  # (2020 sexual transmisson/2019 sexual transmission)
#                                                                                  levels.of.stratification = c(0,1), 
#                                                                                  dimensions = c("sex","race","age"),
#                                                                                  from.year = 2018, 
#                                                                                  observation.correlation.form = 'compound.symmetry', 
#                                                                                  error.variance.term = 0.03, # pick a smarter one
#                                                                                  error.variance.type = 'cv',
#                                                                                  correlation.different.years = 0.5,
#                                                                                  
#                                                                                  # ratio of HIV transmission (year-on-year) is proportional 
#                                                                                  # to ratio of gonorrhea transmission (year-on-year) 
#                                                                                  # with best estimate of 1 but can be off by X%
#                                                                                  included.multiplier = 1,
#                                                                                  included.multiplier.sd = 0.1232992, # see applications/EHE/ehe_sti_included_multiplier.R
#                                                                                  included.multiplier.correlation = 0.5,
#                                                                                  
#                                                                                  weights = list(1), 
#                                                                                  equalize.weight.by.year = F 
#   )

# race.gonorrhea.year.on.year.change.likelihood.instructions = 
#   create.time.lagged.comparison.likelihood.instructions(outcome.for.data = "gonorrhea.ratio", # zoe changing 0-14 to 13-14, throw out 'unknown'
#                                                         outcome.for.sim = "sexual.transmission.rates", 
#                                                         # (2020 gon diagnoses / 2019 gon diagnoses) proportional to 
#                                                         # (2020 sexual transmisson/2019 sexual transmission)
#                                                         levels.of.stratification = c(0,1), 
#                                                         dimensions = c("race"),
#                                                         from.year = 2008, 
#                                                         observation.correlation.form = 'compound.symmetry', 
#                                                         error.variance.term = 0.03, # pick a smarter one
#                                                         error.variance.type = 'cv',
#                                                         correlation.different.years = 0.5,
#                                                         weights = list(1), 
#                                                         equalize.weight.by.year = F 
#   )

#-- YEAR-ON-YEAR SYPHILIS CHANGE ----
ps.syphilis.year.on.year.change.likelihood.instructions = 
  create.basic.ratio.likelihood.instructions(outcome.for.data = "ps.syphilis.ratio", 
                                             outcome.for.sim = "sexual.transmission.rates", 
                                             # (2020 ps diagnoses / 2019 ps diagnoses) proportional to 
                                             # (2020 sexual transmisson/2019 sexual transmission)
                                             levels.of.stratification = c(0,1), 
                                             dimensions = c("sex","race","age"),
                                             from.year = 2018, 
                                             observation.correlation.form = 'compound.symmetry', 
                                             error.variance.term = 0.03, # if we can't find something better, use diagnoses estimate from above (0.07425679)
                                             error.variance.type = 'cv',
                                             correlation.different.years = 0.5,
                                             
                                             ratio.cv = 1.5,
                                             # ratio.correlation = , # NULL will enter default of 0
                                             
                                             weights = (1*TOTAL.WEIGHT),
                                             equalize.weight.by.year = F 
  )

# ps.syphilis.year.on.year.change.likelihood.instructions = 
#   create.time.lagged.comparison.likelihood.instructions.with.included.multiplier(outcome.for.data = "ps.syphilis.ratio", 
#                                                                                  outcome.for.sim = "sexual.transmission.rates", 
#                                                                                  # (2020 ps diagnoses / 2019 ps diagnoses) proportional to 
#                                                                                  # (2020 sexual transmisson/2019 sexual transmission)
#                                                                                  levels.of.stratification = c(0,1), 
#                                                                                  dimensions = c("sex","race","age"),
#                                                                                  from.year = 2018, 
#                                                                                  observation.correlation.form = 'compound.symmetry', 
#                                                                                  error.variance.term = 0.03, # pick a smarter one
#                                                                                  error.variance.type = 'cv',
#                                                                                  correlation.different.years = 0.5,
#                                                                                  
#                                                                                  # ratio of HIV transmission (year-on-year) is proportional 
#                                                                                  # to ratio of ps transmission (year-on-year) 
#                                                                                  # with best estimate of 1 but can be off by X%
#                                                                                  included.multiplier = 1,
#                                                                                  included.multiplier.sd = 0.1232992, # see applications/EHE/ehe_sti_included_multiplier.R
#                                                                                  included.multiplier.correlation = 0.5,
#                                                                                  
#                                                                                  weights = list(1), 
#                                                                                  equalize.weight.by.year = F 
#   )

# race.ps.syphilis.year.on.year.change.likelihood.instructions = 
#   create.time.lagged.comparison.likelihood.instructions(outcome.for.data = "ps.syphilis.ratio", # zoe working on this
#                                                         outcome.for.sim = "sexual.transmission.rates", # we have to define this outcome
#                                                         # (2020 ps diagnoses / 2019 ps diagnoses) proportional to 
#                                                         # (2020 sexual transmisson/2019 sexual transmission)
#                                                         levels.of.stratification = c(0,1), 
#                                                         dimensions = c("race"),
#                                                         from.year = 2008, 
#                                                         observation.correlation.form = 'compound.symmetry', 
#                                                         error.variance.term = 0.03, # pick a smarter one
#                                                         error.variance.type = 'cv',
#                                                         correlation.different.years = 0.2,
#                                                         weights = list(1), 
#                                                         equalize.weight.by.year = F 
#   )

#-- JOIN THE POPULATION-RELATED LIKELIHOODS --#  ----
joint.pop.migration.total.trans.likelihood.instructions = 
  join.likelihood.instructions(population.likelihood.instructions,
                               immigration.likelihood.instructions,
                               emigration.likelihood.instructions,
                               general.mortality.likelihood.instructions,
                               total.prevalence.likelihood.instructions, 
                               total.new.diagnoses.likelihood.instructions
                               ) 

#-- JOIN THE TRANSMISSION-RELATED AND POPULATION LIKELIHOODS --#  ----
transmission.pop.idu.aware.aids.testing.likelihood.instructions = 
  join.likelihood.instructions(race.risk.new.diagnoses.likelihood.instructions, # race/risk only 10/30
                               race.risk.prevalence.likelihood.instructions, # race/risk only 10/30
                               non.age.aids.diagnoses.likelihood.instructions,
                               #proportion.tested.likelihood.instructions, # removing 10/30
                               population.likelihood.instructions,
                               heroin.likelihood.instructions,
                               cocaine.likelihood.instructions,
                               aids.deaths.likelihood.instructions,
                               hiv.mortality.likelihood.instructions # added 11/1
                               #awareness.likelihood.instructions, # removing 10/30
                               #race.ps.syphilis.year.on.year.change.likelihood.instructions,
                               #race.gonorrhea.year.on.year.change.likelihood.instructions
  )


#-- FULL LIKELIHOOD, ADDED AIDS DIAGNOSES BACK --# ---- 
FULL.likelihood.instructions.with.aids =  join.likelihood.instructions(
  # POPULATION LIKELIHOODS
  population.likelihood.instructions, 
  immigration.likelihood.instructions, 
  emigration.likelihood.instructions,
  
  # TRANSMISSION LIKELIHOODS
  new.diagnoses.likelihood.instructions,
  prevalence.likelihood.instructions,
  
  # MORTALITY LIKELIHOODS
  hiv.mortality.likelihood.instructions,
  general.mortality.likelihood.instructions,
  aids.deaths.likelihood.instructions,
  
  # AIDS DIAGNOSES LIKELIHOOD
  non.age.aids.diagnoses.likelihood.instructions,
  
  # CONTINUUM LIKELIHOODS
  proportion.tested.likelihood.instructions,
  hiv.test.positivity.likelihood.instructions, 
  awareness.likelihood.instructions,
  suppression.likelihood.instructions,
  
  # PREP LIKELIHOODS
  prep.uptake.likelihood.instructions,
  prep.indications.likelihood.instructions,
  
  # IDU LIKELIHOODS
  heroin.likelihood.instructions,
  cocaine.likelihood.instructions
)


#-- FULL LIKELIHOOD WITH COVID TESTING LIKELIHOOD ONLY --# ---- 
FULL.likelihood.instructions.with.covid.testing =  join.likelihood.instructions(
  # POPULATION LIKELIHOODS
  population.likelihood.instructions, 
  immigration.likelihood.instructions, 
  emigration.likelihood.instructions,
  
  # TRANSMISSION LIKELIHOODS
  new.diagnoses.likelihood.instructions,
  prevalence.likelihood.instructions,
  
  # MORTALITY LIKELIHOODS
  hiv.mortality.likelihood.instructions,
  general.mortality.likelihood.instructions,
  aids.deaths.likelihood.instructions,
  
  # AIDS DIAGNOSES LIKELIHOOD
  non.age.aids.diagnoses.likelihood.instructions,
  
  # CONTINUUM LIKELIHOODS
  proportion.tested.likelihood.instructions,
  hiv.test.positivity.likelihood.instructions, 
  awareness.likelihood.instructions,
  suppression.likelihood.instructions,
  
  # PREP LIKELIHOODS
  prep.uptake.likelihood.instructions,
  prep.indications.likelihood.instructions,
  
  # IDU LIKELIHOODS
  heroin.likelihood.instructions,
  cocaine.likelihood.instructions,
  
  # COVID TESTING LIKELIHOOD
  number.of.tests.year.on.year.change.likelihood.instructions
  
)

#-- FULL LIKELIHOOD WITH THREE COVID LIKELIHOODS --# ---- 
FULL.likelihood.instructions.with.covid =  join.likelihood.instructions(
  # POPULATION LIKELIHOODS
  population.likelihood.instructions, 
  immigration.likelihood.instructions, 
  emigration.likelihood.instructions,
  
  # TRANSMISSION LIKELIHOODS
  new.diagnoses.likelihood.instructions,
  prevalence.likelihood.instructions,
  
  # MORTALITY LIKELIHOODS
  hiv.mortality.likelihood.instructions,
  general.mortality.likelihood.instructions,
  aids.deaths.likelihood.instructions,
  
  # AIDS DIAGNOSES LIKELIHOOD
  non.age.aids.diagnoses.likelihood.instructions,
  
  # CONTINUUM LIKELIHOODS
  proportion.tested.likelihood.instructions,
  hiv.test.positivity.likelihood.instructions, 
  awareness.likelihood.instructions,
  suppression.likelihood.instructions,
  
  # PREP LIKELIHOODS
  prep.uptake.likelihood.instructions,
  prep.indications.likelihood.instructions,
  
  # IDU LIKELIHOODS
  heroin.likelihood.instructions,
  cocaine.likelihood.instructions,
  
  # COVID LIKELIHOODS
  number.of.tests.year.on.year.change.likelihood.instructions,
  gonorrhea.year.on.year.change.likelihood.instructions,
  ps.syphilis.year.on.year.change.likelihood.instructions
  
)